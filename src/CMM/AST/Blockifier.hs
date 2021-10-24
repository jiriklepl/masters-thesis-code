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
{-# LANGUAGE Rank2Types #-}

module CMM.AST.Blockifier where

import safe Control.Monad.State.Lazy
import safe Control.Lens.Getter
import safe Control.Lens.Setter
import safe Data.Functor
import safe Data.Foldable
import safe Data.Maybe
import safe Data.Tuple
import safe Control.Lens.Type
import safe qualified Data.Map as Map
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)
import safe Prelude hiding (reads)
import safe Prettyprinter (Pretty)

import safe CMM.AST
import safe CMM.AST.Utils
import safe CMM.AST.BlockifierState
import safe CMM.AST.BlockAnnot
import safe CMM.Parser.HasPos
import safe CMM.Utils
import safe CMM.Pretty ()

type MonadBlockify m = (MonadState Blockifier m, MonadIO m)

helperName :: Text -> Text
helperName = addPrefix lrAnalysisPrefix

lrAnalysisPrefix :: Text
lrAnalysisPrefix = "LR"

blockIsSet :: MonadState Blockifier m => m Bool
blockIsSet = uses currentBlock $ not . null

blocksCache :: MonadState Blockifier m => Text -> m Int
blocksCache name = do
  table <- use blocksTable
  case name `Map.lookup` table of
    Just index -> return index
    Nothing -> let index = Map.size table
               in index <$ (blocksTable .= Map.insert name index table)

updateBlock :: MonadState Blockifier m => Maybe Text -> m ()
updateBlock mName = do
  mIndex <- traverse blocksCache mName
  use currentBlock >>= \case
      Just oldName -> do
        cData <- use currentData
        blockData %= Map.insert oldName cData
        currentBlock .= mIndex
        currentData .= mempty
      Nothing -> currentBlock .= mIndex

setBlock :: MonadState Blockifier m => Text -> m ()
setBlock = updateBlock . Just

unsetBlock :: MonadState Blockifier m => m ()
unsetBlock = updateBlock Nothing

noBlockAnnots :: Functor n => n a -> n (a, BlockAnnot)
noBlockAnnots = updateAnnots (, NoBlock)

withNoBlockAnnot :: a -> n (a, BlockAnnot) -> Annot n (a, BlockAnnot)
withNoBlockAnnot a = withAnnot (a, NoBlock)

addControlFlow :: MonadState Blockifier m => Text -> m ()
addControlFlow destBlock =
  use currentBlock >>= \case
    Nothing -> pure ()
    Just block -> do
      index <- blocksCache destBlock
      controlFlow %= ((block, index) :)

class MetadataType t =>
      Register t n
  where
  register :: MonadState Blockifier m => t -> n -> m ()

instance Register ReadsVars Text where
  register _ var =
    currentData %=
      Map.insertWith
        (\_ (reads, writes, lives) -> (not writes || reads, writes, lives))
        var
        (True, False, True)

instance Register WritesVars Text where
  register _ var =
    currentData %=
      Map.insertWith
        (\_ (reads, _, lives) -> (reads, True, lives))
        var
        (False, True, False)

instance (GetMetadata t (n a), Register t Text) => Register t (n a) where
  register t = traverse_ (register t) . getMetadata t

registerReads ::
     (Register ReadsVars n, MonadState Blockifier m) => n -> m ()
registerReads = register ReadsVars

registerWrites ::
     (Register WritesVars n, MonadState Blockifier m) => n -> m ()
registerWrites = register WritesVars

registerReadsWrites ::
     (Register WritesVars n, Register ReadsVars n, MonadState Blockifier m)
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

withBlockAnnot ::
     (HasPos a, MonadBlockify m)
  => Annot Stmt a
  -> m (Annot Stmt (a, BlockAnnot))
withBlockAnnot stmt@(Annot n annot) =
  use currentBlock >>= \case
    Nothing ->
      registerWarning stmt "The statement is unreachable" $>
      withAnnot (annot, Unreachable) (noBlockAnnots n)
    Just block ->
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

instance GetMetadata WritesVars (Formal a) where
  getMetadata _ formal = [getName formal]

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
  getMetadata t (ContStmt _ kindNames) = getMetadata t kindNames
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

blockifyProcedure :: (Blockify n a, MonadBlockifier m) =>
  n a -> m (n (a, BlockAnnot))
blockifyProcedure procedure = blockify procedure <* unsetBlock

class Blockify n a where
  blockify :: MonadBlockify m => n a -> m (n (a, BlockAnnot))

-- TODO: refactor this
instance HasPos a => Blockify (Annot Datum) a where
  blockify datum@(Annot DatumLabel {} _) =
    storeSymbol stackLabels "datum label" datum $> noBlockAnnots datum
  blockify datum@(Annot _ _) =
    return $ noBlockAnnots datum

instance HasPos a => Blockify (Annot Procedure) a where
  blockify (Annot (Procedure mConv name formals body) a) = do
    formals' <- traverse blockify formals
    index <- blocksCache $ helperName "procedure"
    currentBlock ?= index
    traverse_ registerWrites formals
    withAnnot (a, Begins index) .
      Procedure mConv (noBlockAnnots name) formals' <$>
      blockify body

instance HasPos a =>
         Blockify (Annot Body) a where
  blockify (Annot (Body bodyItems) a) =
    withNoBlockAnnot a . Body <$> traverse blockify bodyItems

constructBlockified :: (Blockify (Annot n1) a1, MonadBlockify m) =>
  (Annot n1 (a1, BlockAnnot) -> n2 (a2, BlockAnnot))
  -> a2 -> Annot n1 a1 -> m (Annot n2 (a2, BlockAnnot))
constructBlockified constr a n =  do
    n' <- blockify n
    return . withAnnot (a, snd $ takeAnnot n') $ constr n'

instance HasPos a =>
         Blockify (Annot BodyItem) a where
  blockify (Annot (BodyStmt stmt) a) =
    constructBlockified BodyStmt a stmt
  blockify (Annot (BodyDecl decl) a) =
    constructBlockified BodyDecl a decl
  blockify (Annot (BodyStackDecl stackDecl) a) =
    constructBlockified BodyStackDecl a stackDecl

instance HasPos a => Blockify (Annot StackDecl) a where
  blockify (Annot (StackDecl datums) a) =
    withNoBlockAnnot a . StackDecl <$> traverse blockify datums

instance HasPos a => Blockify (Annot Decl) a where
  blockify (Annot (RegDecl invar regs) a) =
    constructBlockified (RegDecl invar) a regs
  blockify (Annot (ImportDecl imports') a) =
    withNoBlockAnnot a . ImportDecl <$> traverse blockify imports'
  blockify decl@(Annot ConstDecl {} _) =
    storeSymbol constants "constant declaration" decl $> noBlockAnnots decl
  blockify decl@(Annot _ _) = return $ noBlockAnnots decl

instance HasPos a => Blockify (Annot Import) a where
  blockify import'@(Annot Import {} _) =
    storeSymbol imports "import" import' $> noBlockAnnots import'

instance HasPos a => Blockify (Annot Registers) a where
  blockify regs@(Annot (Registers _ _ nameStrLits) _) =
    traverse_ (storeRegister . fst) nameStrLits $> noBlockAnnots regs

instance HasPos a => Blockify (Annot Formal) a where
  blockify formal = storeRegister formal $> noBlockAnnots formal

storeRegister :: (MonadState Blockifier m, HasName n, HasPos n, Pretty n, MonadIO m) => n -> m ()
storeRegister = storeSymbol registers "register"

storeSymbol :: (MonadState Blockifier m, HasName n, HasPos n, Pretty n, MonadIO m) => Lens Blockifier Blockifier (Set Text) (Set Text) -> Text -> n -> m ()
storeSymbol symbolSet symbolName node = do
  symbols' <- use symbolSet
  if getName node `Set.member` symbols' then
      registerError node ("Duplicate " <> symbolName)
  else symbolSet .= getName node `Set.insert` symbols'

instance HasPos a => Blockify (Annot Stmt) a where
  blockify stmt@(Annot LabelStmt {} _) = do
    addControlFlow $ getName stmt -- a possible fallthrough
    storeSymbol labels "label" stmt
    blockifyLabelStmt stmt
  blockify stmt@(Annot ContStmt {} _) = do
    storeSymbol continuations "continuation" stmt
    blockIsSet >>=
      (`when` registerError stmt "Fallthrough to a continuation is forbidden")
    blockifyLabelStmt stmt <* registerWrites stmt
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
  blockify stmt@(Annot (CallStmt _ _ _ _ _ callAnnots) _) = -- TODO: implement `cut to` statements
    registerReads stmt *> withBlockAnnot stmt <*
    when (neverReturns callAnnots) unsetBlock

-- TODO: make it clearer that this is a logic-error inside of the compiler
-- This is here just for completeness
flatteningError :: (HasPos n, Pretty n, MonadBlockify m) => n -> m ()
flatteningError stmt =
  registerError stmt "Compilation failure in the flattening phase"

blockifyLabelStmt ::
     (MonadState Blockifier m, Functor n, HasName (n a))
  => Annot n a
  -> m (Annot n (a, BlockAnnot))
blockifyLabelStmt (Annot stmt a) = do
  let name = getName stmt
  setBlock name
  index <- blocksCache name -- TODO: this is not optimal
  return . withAnnot (a, Begins index) $ noBlockAnnots stmt
