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
{-# LANGUAGE NamedFieldPuns #-}

module Language.AST.LRAnalysis where

import safe Control.Monad.State.Lazy
import safe Data.Functor
import safe Data.Foldable
import safe Data.Maybe
import safe Data.Tuple
import safe qualified Data.Graph as Graph
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)
import safe qualified Data.Text as T
import safe qualified Data.Text.IO as T
import safe Prelude hiding (reads)
import safe Prettyprinter (Pretty, pretty)
import safe Text.Megaparsec.Pos (SourcePos)

import safe Language.AST
import safe Language.AST.Utils
import safe Language.Utils
import safe Language.Pretty ()
import safe Language.Warnings
import safe qualified Peano

data BlockAnnot
  = PartOf Int
  | Begins Text
  | Unreachable
  | NoBlock
  deriving (Show)

type MonadBlockify m = (MonadState BlockifierState m, MonadIO m)

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

type HasPosAnnot9 n1 n2 n3 n4 n5 n6 n7 n8 n9 a
   = (HasPosAnnot n1 a, HasPosAnnot8 n2 n3 n4 n5 n6 n7 n8 n9 a)

type HasPosAnnot10 n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 a
   = (HasPosAnnot n1 a, HasPosAnnot9 n2 n3 n4 n5 n6 n7 n8 n9 n10 a)

instance HasPos (Annot n SourcePos) where
  getPos = takeAnnot

data BlockifierState =
  Blockifier
    { controlFlow :: [(Int, Int)]
    , blocksTable :: Map Text Int
    , currentBlock :: Maybe Int
    , currentData :: BlockVars
    , blockData :: BlockData
    , registers :: Set Text
    , imports :: Set Text
    , constants :: Set Text
    , stackLabels :: Set Text
    , labels :: Set Text
    , continuations :: Set Text
    , errors :: Int
    , warnings :: Int
    }
  deriving (Show)

type BlockData = Map Int BlockVars
type BlockVars = Map Text (Bool, Bool, Bool)

initBlockifier :: BlockifierState
initBlockifier =
  Blockifier
    { controlFlow = mempty
    , blocksTable = mempty
    , currentBlock = Nothing
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

helperName :: Text -> Text
helperName = addPrefix lrAnalysisPrefix

lrAnalysisPrefix :: Text
lrAnalysisPrefix = "LR"

registerError :: (HasPos n, Pretty n, MonadBlockify m) => n -> Text -> m ()
registerError node message = do
  modify $ \s -> s {errors = errors s + 1}
  makeMessage mkError node message

registerWarning :: (HasPos n, Pretty n, MonadBlockify m) => n -> Text -> m ()
registerWarning node message = do
  modify $ \s -> s {warnings = warnings s + 1}
  makeMessage mkWarning node message

makeMessage :: (HasPos n, Pretty n, MonadBlockify m) => (SourcePos -> Text -> Text) -> n -> Text -> m ()
makeMessage constructor node message = do
  let pos = getPos node
  liftIO . T.putStrLn $ constructor pos $ (T.pack . show $ pretty node) <> "\n\t" <> message

blockIsSet :: MonadState BlockifierState m => m Bool
blockIsSet = not . null <$> gets currentBlock

blocksCache :: MonadState BlockifierState m => Text -> m Int
blocksCache name = do
  table <- gets blocksTable
  case name `Map.lookup` table of
    Just index -> return index
    Nothing -> let index = Map.size table
               in index <$ modify (\s -> s {blocksTable = Map.insert name index table})

updateBlock :: MonadState BlockifierState m => Maybe Text -> m ()
updateBlock mName = do
  mIndex <- traverse blocksCache mName
  modify $ \s ->
    case currentBlock s of
      Just oldName ->
        s
          { blockData = Map.insert oldName (currentData s) (blockData s)
          , currentBlock = mIndex
          , currentData = mempty
          }
      Nothing -> s {currentBlock = mIndex}

setBlock :: MonadState BlockifierState m => Text -> m ()
setBlock = updateBlock . Just

unsetBlock :: MonadState BlockifierState m => m ()
unsetBlock = updateBlock Nothing

noBlockAnnots :: Functor n => n a -> n (a, BlockAnnot)
noBlockAnnots = updateAnnots (, NoBlock)

withNoBlockAnnot :: a -> n (a, BlockAnnot) -> Annot n (a, BlockAnnot)
withNoBlockAnnot a = withAnnot (a, NoBlock)

addControlFlow :: MonadState BlockifierState m => Text -> m ()
addControlFlow destBlock = do
  index <- blocksCache destBlock
  gets currentBlock >>= \case
    Nothing -> pure ()
    Just block ->
      modify $ \s -> s {controlFlow = (block, index) : controlFlow s}

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
     (HasPosAnnot Stmt a, MonadBlockify m)
  => Annot Stmt a
  -> m (Annot Stmt (a, BlockAnnot))
withBlockAnnot stmt@(Annot n annot) =
  gets currentBlock >>= \case
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

blockifyProcedure ::
     HasPosAnnot10 Formal Name Procedure BodyItem Datum Import StackDecl Stmt Body Decl a
  => Annot Procedure a
  -> IO (Annot Procedure (a, BlockAnnot), BlockifierState)
blockifyProcedure procedure =
  flip runStateT initBlockifier $ do
    index <- blocksCache $ helperName "procedure"
    modify $ \s -> s {currentBlock = Just index}
    blockify procedure <* unsetBlock <* analyzeFlow procedure

class Blockify n a where
  blockify :: MonadBlockify m => n a -> m (n (a, BlockAnnot))

-- TODO: refactor this
instance HasPos (Annot Datum a) => Blockify (Annot Datum) a where
  blockify datum@(Annot DatumLabel {} _) = do
    let name = getName datum
    sls <- gets stackLabels
    if name `Set.member` sls then
      registerError datum "Duplicate datum label"
    else
      modify $ \s -> s {stackLabels = name `Set.insert` sls}
    return $ noBlockAnnots datum
  blockify datum@(Annot _ _) = do
    return $ noBlockAnnots datum

instance HasPosAnnot9 Formal Name BodyItem Stmt Body Decl StackDecl Import Datum a =>
         Blockify (Annot Procedure) a where
  blockify (Annot (Procedure mConv name formals body) a) = do
    formals' <- traverse blockify formals
    withNoBlockAnnot a .
      Procedure mConv (noBlockAnnots name) formals' <$>
      blockify body

instance HasPosAnnot8 Name BodyItem Stmt Decl Body StackDecl Import Datum a =>
         Blockify (Annot Body) a where
  blockify (Annot (Body bodyItems) a) =
    withNoBlockAnnot a . Body <$> traverse blockify bodyItems

instance HasPosAnnot8 Name BodyItem Stmt Body Decl StackDecl Import Datum a =>
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

-- TODO: refactor this
instance HasPosAnnot3 Import Decl Name a => Blockify (Annot Decl) a where
  blockify (Annot (RegDecl invar regs) a) =
    withNoBlockAnnot a . RegDecl invar <$> blockify regs
  blockify (Annot (ImportDecl imports') a) =
    withNoBlockAnnot a . ImportDecl <$> traverse blockify imports'
  blockify decl@(Annot ConstDecl {} _) = do
    let name = getName decl
    cs <- gets imports
    if name `Set.member` cs then
      registerError decl "Duplicate constant declaration"
    else
      modify $ \s -> s {constants = name `Set.insert` cs}
    return $ noBlockAnnots decl
  blockify decl@(Annot _ _) = return $ noBlockAnnots decl

instance HasPosAnnot Import a => Blockify (Annot Import) a where
  blockify import'@(Annot Import {} _) = do
    let name = getName import'
    is <- gets imports
    if name `Set.member` is then
      registerError import' "Duplicate import"
    else
      modify $ \s -> s {imports = name `Set.insert` is}
    return $ noBlockAnnots import'

instance HasPosAnnot Name a => Blockify (Annot Registers) a where
  blockify regs@(Annot (Registers _ _ nameStrLits) _) =
    traverse_ (registerRegister . fst) nameStrLits $> noBlockAnnots regs

instance HasPosAnnot Formal a => Blockify (Annot Formal) a where
  blockify formal =
    registerRegister formal $> noBlockAnnots formal

registerRegister :: (MonadState BlockifierState m, HasName n, HasPos n, Pretty n,
  MonadIO m) => n -> m ()
registerRegister name = do
  Blockifier{registers} <- get
  if getName name `Set.member` registers then
      registerError name "Duplicate register"
  else modify $ \s -> s {registers = getName name `Set.insert` registers}

instance HasPosAnnot8 Name Stmt BodyItem Body Decl StackDecl Import Datum a =>
         Blockify (Annot Stmt) a where
  blockify stmt@(Annot LabelStmt {} _) = do
    let name = getName stmt
    ls <- gets labels
    addControlFlow name -- a possible fallthrough
    if name `Set.member` ls then
      registerError stmt "Duplicate label"
    else
      modify $ \s -> s {labels = name `Set.insert` ls}
    blockifyLabelStmt stmt
  blockify stmt@(Annot ContStmt {} _) = do
    let name = getName stmt
    cs <- gets continuations
    if name `Set.member` cs then
      registerError stmt "Duplicate continuation"
    else
      modify $ \s -> s {continuations = name `Set.insert` cs}
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

-- This is here just for completeness
flatteningError :: (HasPos n, Pretty n, MonadBlockify m) => n -> m ()
flatteningError stmt =
  registerError stmt "Compilation failure in the flattening phase"

analyzeFlow :: (HasPosAnnot Procedure a, MonadBlockify m) => Annot Procedure a -> m ()
analyzeFlow procedure@(Annot _ _) = do
  -- TODO: implement `cut to` statements
  Blockifier
    { registers = regs
    , blockData = bData
    , labels = labs
    , continuations = conts
    , blocksTable = blocks
    , controlFlow = flow
    } <- get
  let graph = Graph.buildG (0, Map.size blocks - 1) flow -- one block is guaranteed (procedure)
      reachable = Set.fromList $ Graph.reachable graph 0
      blockNames = Map.fromList $ swap <$> Map.toList blocks
      unreachableLabels = labs Set.\\ Set.map (blockNames Map.!) reachable
      unreachableContinuations = conts Set.\\ Set.map (blockNames Map.!) reachable
      labelsWarning = T.unwords . filter hasPrefix $ Set.toList unreachableLabels
      continuationsWarning = T.unwords . filter hasPrefix $ Set.toList unreachableContinuations
  unless (T.null labelsWarning && T.null continuationsWarning) .
    registerWarning procedure $
      "Unreachable labels: " <> labelsWarning <> "\n\t" <>
      "Unreachable continuations: " <> continuationsWarning -- TODO: maybe split this
  let preCleanData = (`Map.restrictKeys` regs) <$> Map.restrictKeys bData reachable -- we filter out variables that are not local variables and whole blocks that are not reachable
      allVars = (False, False, False) <$ Map.foldlWithKey (\vars _ block -> block `Map.union` vars) mempty preCleanData
      cleanData = (`Map.union` allVars) <$> preCleanData -- we make sure that each block has a record for each variable
      cleanFlow = filter ((`Set.member` reachable) . fst) flow -- we filter out unreachable flow
  modify $ \s -> s { blockData = cleanData }
  doWhile $ or <$> traverse updateFlowPair cleanFlow
  uninitialized <- Map.keys . Map.filter (not . get_nth Peano.p2) . (Map.! 0) <$> gets blockData
  unless (null uninitialized) $ registerError procedure ("Uninitialized registers: " <> T.unwords uninitialized)

-- TODO: "unused after write" warning

updateFlowPair :: MonadBlockify m => (Int, Int) -> m Bool
updateFlowPair (from, to) = do
  blocks <- gets blockData
  let toVars = blocks Map.! to
      fromVars = blocks Map.! from
      (Just newBlock, newBlocks) = Map.insertLookupWithKey (\_ new old -> Map.fromAscList $ updateFlowPairVar <$> zip (Map.toAscList old) (Map.toAscList new)) from toVars blocks
  modify $ \s -> s { blockData = newBlocks }
  return . or $ zipWith (\a b -> get_nth Peano.p2 a /= get_nth Peano.p2 b) (Map.elems newBlock) (Map.elems fromVars)
  where updateFlowPairVar ((name, (r, w, l)), (_, (_, _, l'))) = (name, (r, w, l || (not w && l')))

blockifyLabelStmt ::
     (MonadState BlockifierState m, Functor n, HasName (n a))
  => Annot n a
  -> m (Annot n (a, BlockAnnot))
blockifyLabelStmt (Annot stmt a) = do
  let name = getName stmt
  setBlock name
  return . withAnnot (a, Begins name) $ noBlockAnnots stmt
