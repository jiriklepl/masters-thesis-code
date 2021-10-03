{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Language.AST.LRAnalysis where

import safe Control.Monad.State.Lazy
import safe Data.Foldable
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Text (Text)
import safe qualified Data.Text as T
import safe qualified Data.Text.IO as T
import safe Prettyprinter (pretty)
import safe Text.Megaparsec.Pos (SourcePos)

import safe Language.AST
import safe Language.AST.Utils
import safe Language.Pretty ()
import safe Language.Warnings
import safe Prelude hiding (reads)

data BlockAnnot
  = PartOf Text
  | Begins Text
  | Unreachable
  | NoBlock
  deriving (Show)

type MonadBlockify n m = (HasPos n, MonadState BlockifierState m, MonadIO m)

class HasPos n where
  getPos :: n -> SourcePos

type HasPosAnnot n a = HasPos (Annot n a)

type HasPosAnnot1 n a = HasPosAnnot n a

type HasPosAnnot2 n1 n2 a = (HasPosAnnot1 n1 a, HasPosAnnot1 n2 a)

type HasPosAnnot3 n1 n2 n3 a = (HasPosAnnot n1 a, HasPosAnnot2 n2 n3 a)

type HasPosAnnot4 n1 n2 n3 n4 a = (HasPosAnnot n1 a, HasPosAnnot3 n2 n3 n4 a)

type HasPosAnnot5 n1 n2 n3 n4 n5 a
   = (HasPosAnnot n1 a, HasPosAnnot4 n2 n3 n4 n5 a)

type HasPosAnnot6 n1 n2 n3 n4 n5 n6 a
   = (HasPosAnnot n1 a, HasPosAnnot5 n2 n3 n4 n5 n6 a)

type HasPosAnnot7 n1 n2 n3 n4 n5 n6 n7 a
   = (HasPosAnnot n1 a, HasPosAnnot6 n2 n3 n4 n5 n6 n7 a)

type HasPosAnnot8 n1 n2 n3 n4 n5 n6 n7 n8 a
   = (HasPosAnnot n1 a, HasPosAnnot7 n2 n3 n4 n5 n6 n7 n8 a)

instance HasPos (Annot n SourcePos) where
  getPos = takeAnnot

data BlockifierState =
  Blockifier
    { controlFlow :: [(Text, Text)]
    , currentBlock :: Maybe Text
    , currentData :: Map Text (Bool, Bool, Bool)
    , blockData :: Map Text (Map Text (Bool, Bool, Bool))
    , registers :: [Text]
    , imports :: [Text]
    , constants :: [Text]
    , stackLabels :: [Text]
    , labels :: [Text]
    , continuations :: [Text]
    , errors :: Int
    , warnings :: Int
    }
  deriving (Show)

initBlockifier :: BlockifierState
initBlockifier =
  Blockifier
    { controlFlow = mempty
    , currentBlock = mempty
    , currentData = mempty
    , blockData = mempty
    , registers = mempty
    , imports = mempty
    , constants = mempty
    , stackLabels = mempty
    , labels = mempty
    , continuations = mempty
    , errors = 0
    , warnings = 0
    }

registerError :: MonadBlockify n m => n -> Text -> m ()
registerError node message = do
  let pos = getPos node
  modify $ \s -> s {errors = errors s + 1}
  liftIO . T.putStrLn $ mkError pos message

registerWarning :: MonadBlockify n m => n -> Text -> m ()
registerWarning node message = do
  let pos = getPos node
  modify $ \s -> s {warnings = warnings s + 1}
  liftIO . T.putStrLn $ mkWarning pos message

blockIsSet :: MonadState BlockifierState m => m Bool
blockIsSet = not . null <$> gets currentBlock

updateBlock :: MonadState BlockifierState m => Maybe Text -> m ()
updateBlock mName = do
  modify $ \s ->
    case currentBlock s of
      Just oldName ->
        s
          { blockData = Map.insert oldName (currentData s) (blockData s)
          , currentBlock = mName
          , currentData = mempty
          }
      Nothing -> s {currentBlock = mName}

setBlock :: MonadState BlockifierState m => Text -> m ()
setBlock = updateBlock . Just

unsetBlock :: MonadState BlockifierState m => m ()
unsetBlock = updateBlock Nothing

noBlockAnnots :: Functor n => n a -> n (a, BlockAnnot)
noBlockAnnots = updateAnnots (, NoBlock)

withNoBlockAnnot :: a -> n (a, BlockAnnot) -> Annot n (a, BlockAnnot)
withNoBlockAnnot a = withAnnot (a, NoBlock)

addControlFlow :: MonadState BlockifierState m => Text -> m ()
addControlFlow destBlock =
  gets currentBlock >>= \case
    Nothing -> pure ()
    Just block ->
      modify $ \s -> s {controlFlow = (block, destBlock) : controlFlow s}

class MetadataType t =>
      Register t n
  where
  register :: MonadState BlockifierState m => t -> n -> m ()

instance Register ReadsVars Text where
  register _ var =
    modify $ \s ->
      s
        { currentData =
            Map.insertWith
              (\_ (reads, writes, lives) -> (not writes || reads, writes, lives))
              var
              (True, False, True)
              (currentData s)
        }

instance Register WritesVars Text where
  register _ var =
    modify $ \s ->
      s
        { currentData =
            Map.insertWith
              (\_ (reads, _, lives) -> (reads, True, lives))
              var
              (False, True, False)
              (currentData s)
        }

instance (GetMetadata t (n a), Register t Text) => Register t (n a) where
  register t = traverse_ (register t) . getMetadata t

registerReads ::
     (Register ReadsVars n, MonadState BlockifierState m) => n -> m ()
registerReads = register ReadsVars

registerWrites ::
     (Register WritesVars n, MonadState BlockifierState m) => n -> m ()
registerWrites = register WritesVars

registerReadsWrites ::
     (Register WritesVars n, Register ReadsVars n, MonadState BlockifierState m)
  => n
  -> m ()
registerReadsWrites n = registerReads n *> registerWrites n

class NeverReturns n where
  neverReturns :: n -> Bool

instance NeverReturns (n a) => NeverReturns (Annot n a) where
  neverReturns (Annot n _) = neverReturns n

instance NeverReturns (n a) => NeverReturns [n a] where
  neverReturns = any neverReturns

instance NeverReturns (CallAnnot a) where
  neverReturns (FlowAnnot flow) = neverReturns flow
  neverReturns AliasAnnot {} = False

instance NeverReturns (Flow a) where
  neverReturns NeverReturns = True
  neverReturns _ = False

class GetTargetNames n t | n -> t where
  getTargetNames :: n -> t

instance GetTargetNames (n a) b => GetTargetNames (Annot n a) b where
  getTargetNames (Annot n _) = getTargetNames n

instance GetTargetNames (Body a) (Maybe [Text]) where
  getTargetNames (Body [bodyItem]) = getTargetNames bodyItem
  getTargetNames _ = error "Does not have targets"

instance GetTargetNames (BodyItem a) (Maybe [Text]) where
  getTargetNames (BodyStmt stmt) = getTargetNames stmt
  getTargetNames _ = error "Does not have targets"

instance GetTargetNames (Stmt a) (Maybe [Text]) where
  getTargetNames (GotoStmt _ mTargets) = getTargetNames <$> mTargets
  getTargetNames (CallStmt _ _ _ _ mTargets _) = getTargetNames <$> mTargets
  getTargetNames (JumpStmt _ _ _ mTargets) = getTargetNames <$> mTargets
  getTargetNames _ = error "Does not have targets"

instance GetTargetNames (Targets a) [Text] where
  getTargetNames (Targets names) = getName <$> names

-- | Returns Nothing on failure
class GetTrivialGotoTarget n where
  getTrivialGotoTarget :: n -> Maybe Text

instance GetTrivialGotoTarget (n a) => GetTrivialGotoTarget (Annot n a) where
  getTrivialGotoTarget (Annot n _) = getTrivialGotoTarget n

instance GetTrivialGotoTarget (Arm a) where
  getTrivialGotoTarget (Arm _ body) = getTrivialGotoTarget body

instance GetTrivialGotoTarget (BodyItem a) where
  getTrivialGotoTarget (BodyStmt stmt) = getTrivialGotoTarget stmt
  getTrivialGotoTarget _ = Nothing

instance GetTrivialGotoTarget (Body a) where
  getTrivialGotoTarget (Body [bodyItem]) = getTrivialGotoTarget bodyItem
  getTrivialGotoTarget _ = Nothing

instance GetTrivialGotoTarget (Stmt a) where
  getTrivialGotoTarget (GotoStmt expr _) = getExprLVName expr
  getTrivialGotoTarget _ = Nothing

withBlockAnnot ::
     MonadBlockify (Annot Stmt a) m
  => Annot Stmt a
  -> m (Annot Stmt (a, BlockAnnot))
withBlockAnnot stmt@(Annot n annot) =
  gets currentBlock >>= \case
    Nothing -> do
      registerWarning stmt "The statement is unreachable"
      return . withAnnot (annot, Unreachable) $ noBlockAnnots n
    Just block -> do
      return . withAnnot (annot, PartOf block) $ noBlockAnnots n

class MetadataType a

data ReadsVars =
  ReadsVars
  deriving (MetadataType, Eq)

data WritesVars =
  WritesVars
  deriving (MetadataType, Eq)

data DeclaresVars =
  DeclaresVars
  deriving (MetadataType, Eq)

class MetadataType t =>
      GetMetadata t n
  where
  getMetadata :: t -> n -> [Text]

instance GetMetadata t n => GetMetadata t (Maybe n) where
  getMetadata t = maybe [] $ getMetadata t

instance GetMetadata t n => GetMetadata t [n] where
  getMetadata t ns = concat $ getMetadata t <$> ns

instance GetMetadata t (n a) => GetMetadata t (Annot n a) where
  getMetadata t (Annot n _) = getMetadata t n

instance GetMetadata DeclaresVars (Decl a) where
  getMetadata t (RegDecl _ regs) = getMetadata t regs
  getMetadata _ _ = []

instance GetMetadata DeclaresVars (Registers a) where
  getMetadata _ (Registers _ _ nameStrLits) = getName . fst <$> nameStrLits

instance (GetMetadata t (BodyItem a), MetadataType t) =>
         GetMetadata t (Body a) where
  getMetadata t (Body bodyItems) = getMetadata t bodyItems

instance GetMetadata ReadsVars (BodyItem a) where
  getMetadata t (BodyStmt stmt) = getMetadata t stmt
  getMetadata _ _ = []

instance GetMetadata WritesVars (BodyItem a) where
  getMetadata t (BodyStmt stmt) = getMetadata t stmt
  getMetadata _ _ = []

instance GetMetadata DeclaresVars (BodyItem a) where
  getMetadata t (BodyDecl stackDecl) = getMetadata t stackDecl
  getMetadata _ BodyStackDecl {} = []
  getMetadata t (BodyStmt stmt) = getMetadata t stmt

instance GetMetadata ReadsVars (Actual a) where
  getMetadata t (Actual _ expr) = getMetadata t expr

instance GetMetadata ReadsVars (Stmt a) where
  getMetadata _ EmptyStmt = []
  getMetadata t (IfStmt expr tBody eBody) =
    getMetadata t expr <> getMetadata t tBody <> getMetadata t eBody
  getMetadata t (SwitchStmt expr arms) =
    getMetadata t expr <> getMetadata t arms
  getMetadata t (SpanStmt key value body) =
    getMetadata t key <> getMetadata t value <> getMetadata t body
  getMetadata t (AssignStmt _ exprs) = getMetadata t exprs
  getMetadata t (PrimOpStmt _ _ actuals _) = getMetadata t actuals
  getMetadata t (CallStmt _ _ expr actuals _ _) =
    getMetadata t expr <> getMetadata t actuals
  getMetadata t (JumpStmt _ expr actuals _) =
    getMetadata t expr <> getMetadata t actuals
  getMetadata t (ReturnStmt _ _ actuals) = getMetadata t actuals
  getMetadata _ LabelStmt {} = []
  getMetadata _ ContStmt {} = []
  getMetadata t (GotoStmt expr _) = getMetadata t expr
  getMetadata t (CutToStmt expr actuals _) =
    getMetadata t expr <> getMetadata t actuals

instance GetMetadata WritesVars (Stmt a) where
  getMetadata t (IfStmt _ tBody eBody) =
    getMetadata t tBody <> getMetadata t eBody
  getMetadata t (SwitchStmt _ arms) = getMetadata t arms
  getMetadata t (SpanStmt _ _ body) = getMetadata t body
  getMetadata t (AssignStmt lvalues _) = getMetadata t lvalues
  getMetadata _ (PrimOpStmt name _ _ _) = [getName name]
  getMetadata t (CallStmt kindNames _ _ _ _ _) = getMetadata t kindNames
  getMetadata _ _ = []

instance GetMetadata DeclaresVars (Stmt a) where
  getMetadata t (IfStmt _ tBody eBody) =
    getMetadata t tBody <> getMetadata t eBody
  getMetadata t (SwitchStmt _ arms) = getMetadata t arms
  getMetadata t (SpanStmt _ _ body) = getMetadata t body
  getMetadata _ _ = []

instance GetMetadata WritesVars (KindName a) where
  getMetadata _ kindName = [getName kindName]

instance GetMetadata ReadsVars (LValue a) where
  getMetadata _ (LVName name) = [getName name]
  getMetadata t (LVRef _ expr _) = getMetadata t expr

instance GetMetadata WritesVars (LValue a) where
  getMetadata _ (LVName name) = [getName name]
  getMetadata _ LVRef {} = []

instance GetMetadata ReadsVars (Expr a) where
  getMetadata _ LitExpr {} = []
  getMetadata t (LVExpr lvalue) = getMetadata t lvalue
  getMetadata t (ParExpr expr) = getMetadata t expr
  getMetadata t (BinOpExpr _ left right) =
    getMetadata t left <> getMetadata t right
  getMetadata t (ComExpr expr) = getMetadata t expr
  getMetadata t (NegExpr expr) = getMetadata t expr
  getMetadata t (InfixExpr _ left right) =
    getMetadata t left <> getMetadata t right
  getMetadata t (PrefixExpr _ actuals) = getMetadata t actuals

instance GetMetadata ReadsVars (Arm a) where
  getMetadata t (Arm ranges body) = getMetadata t ranges <> getMetadata t body

instance GetMetadata ReadsVars (Range a) where
  getMetadata t (Range left right) = getMetadata t left <> getMetadata t right

instance GetMetadata WritesVars (Arm a) where
  getMetadata t (Arm _ body) = getMetadata t body

instance GetMetadata DeclaresVars (Arm a) where
  getMetadata t (Arm _ body) = getMetadata t body

blockifyProcedure ::
     HasPosAnnot8 Procedure BodyItem Datum Import StackDecl Stmt Body Decl a
  => Annot Procedure a
  -> IO (Annot Procedure (a, BlockAnnot), BlockifierState)
blockifyProcedure procedure =
  flip runStateT initBlockifier $ do
    modify $ \s -> s {currentBlock = Just "@procedure"}
    blockify procedure

class Blockify n a where
  blockify :: MonadBlockify (n a) m => n a -> m (n (a, BlockAnnot))

instance Blockify (Annot Datum) a where
  blockify datum@(Annot DatumLabel {} _) = do
    modify $ \s -> s {stackLabels = getName datum : stackLabels s}
    return $ noBlockAnnots datum
  blockify datum@(Annot _ _) = do
    return $ noBlockAnnots datum

instance HasPosAnnot7 BodyItem Stmt Body Decl StackDecl Import Datum a =>
         Blockify (Annot Procedure) a where
  blockify (Annot (Procedure mConv name formals body) a) =
    withNoBlockAnnot a .
    Procedure mConv (noBlockAnnots name) (noBlockAnnots <$> formals) <$>
    blockify body

instance HasPosAnnot7 BodyItem Stmt Decl Body StackDecl Import Datum a =>
         Blockify (Annot Body) a where
  blockify (Annot (Body bodyItems) a) =
    withNoBlockAnnot a . Body <$> traverse blockify bodyItems

instance HasPosAnnot6 Stmt Body Decl StackDecl Import Datum a =>
         Blockify (Annot BodyItem) a where
  blockify (Annot (BodyStmt bodyStmt) a) =
    withNoBlockAnnot a . BodyStmt <$> blockify bodyStmt
  blockify (Annot (BodyDecl decl) a) =
    withNoBlockAnnot a . BodyDecl <$> blockify decl
  blockify (Annot (BodyStackDecl stackDecl) a) =
    withNoBlockAnnot a . BodyStackDecl <$> blockify stackDecl

instance HasPosAnnot Datum a => Blockify (Annot StackDecl) a where
  blockify (Annot (StackDecl datums) a) =
    withNoBlockAnnot a . StackDecl <$> traverse blockify datums

instance HasPosAnnot Import a => Blockify (Annot Decl) a where
  blockify (Annot (ImportDecl imports') a) =
    withNoBlockAnnot a . ImportDecl <$> traverse blockify imports'
  blockify decl@(Annot ConstDecl {} _) = do
    modify $ \s -> s {constants = getName decl : constants s}
    return $ noBlockAnnots decl
  blockify decl@(Annot _ _) = return $ noBlockAnnots decl

instance Blockify (Annot Import) a where
  blockify import_@(Annot Import {} _) = do
    modify $ \s -> s {imports = getName import_ : imports s}
    return $ noBlockAnnots import_

instance HasPosAnnot6 BodyItem Body Decl StackDecl Import Datum a =>
         Blockify (Annot Stmt) a where
  blockify stmt@(Annot LabelStmt {} _) = do
    modify $ \s -> s {labels = getName stmt : labels s}
    blockifyLabelStmt stmt
  blockify stmt@(Annot ContStmt {} _) = do
    modify $ \s -> s {continuations = getName stmt : continuations s}
    blockIsSet >>=
      (`when` registerError stmt "Fallthrough to a continuation is forbidden")
    blockifyLabelStmt stmt
  blockify stmt@(Annot (GotoStmt expr _) _) = do
    case (getExprLVName expr, getTargetNames stmt) of
      (Nothing, Just targets@(_:_)) -> traverse_ addControlFlow targets
      (Just name, Just targets@(_:_)) ->
        if name `elem` targets
          then addControlFlow name
          else traverse_ addControlFlow targets
      (Just name, _) -> addControlFlow name
      (Nothing, _) ->
        registerError
          stmt
          "Indirect goto statement without specified targets is illegal"
    registerReads stmt *> withBlockAnnot stmt <* unsetBlock
  blockify (Annot CutToStmt {} _) =
    error "Cut to statements are not currently implemented" -- TODO: implement `cut to` statements
  blockify stmt@(Annot ReturnStmt {} _) =
    registerReads stmt *> withBlockAnnot stmt <* unsetBlock
  blockify stmt@(Annot JumpStmt {} _) =
    registerReads stmt *> withBlockAnnot stmt <* unsetBlock
  blockify stmt@(Annot EmptyStmt {} _) =
    withBlockAnnot stmt -- This should be completely redundant, included just for completeness
  blockify stmt@(Annot AssignStmt {} _) =
    registerReadsWrites stmt *> withBlockAnnot stmt
  blockify stmt@(Annot PrimOpStmt {} _) -- TODO: In the future, this may end a basic block if given `NeverReturns` flow annotation
   = registerReadsWrites stmt *> withBlockAnnot stmt
  blockify stmt@(Annot (IfStmt _ tBody mEBody) _) = do
    case (getTrivialGotoTarget tBody, getTrivialGotoTarget <$> mEBody) of
      (Just left, Just (Just right)) -> do
        addControlFlow left
        addControlFlow right
      (Just left, Nothing) -> do
        addControlFlow left
      _ -> flatteningError stmt
    withBlockAnnot stmt <* unsetBlock
  blockify stmt@(Annot (SwitchStmt _ arms) _) = do
    case traverse getTrivialGotoTarget arms of
      Just names -> traverse_ addControlFlow names
      Nothing -> flatteningError stmt
    withBlockAnnot stmt <* unsetBlock
  blockify (Annot (SpanStmt key value body) a) =
    withNoBlockAnnot a . SpanStmt (noBlockAnnots key) (noBlockAnnots value) <$>
    blockify body
  blockify stmt@(Annot (CallStmt _ _ _ _ _ callAnnots) _) =
    registerReads stmt *> withBlockAnnot stmt <*
    when (neverReturns callAnnots) unsetBlock

-- This is here just for completeness
flatteningError :: (MonadBlockify n m) => n -> m ()
flatteningError stmt =
  registerError stmt "Compilation failure in the flattening phase"

blockifyLabelStmt ::
     (MonadState BlockifierState m, Functor n, HasName (n a))
  => Annot n a
  -> m (Annot n (a, BlockAnnot))
blockifyLabelStmt (Annot stmt a) = do
  let name = getName stmt
  setBlock name
  return . withAnnot (a, Begins name) $ noBlockAnnots stmt
