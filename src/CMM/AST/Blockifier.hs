{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE Rank2Types #-}

module CMM.AST.Blockifier where

import safe Control.Applicative (Applicative((*>), (<*), pure))
import safe Control.Lens.Getter (use, uses)
import safe Control.Lens.Setter ((%=), (.=), (?=))
import safe Control.Lens.Type (Lens)
import safe Control.Monad (Monad((>>=), return))
import safe Control.Monad.State (when)
import safe Data.Bool (Bool(False, True), (||), not)
import safe Data.Eq (Eq)
import safe Data.Foldable (Foldable(elem, null), any, concat, traverse_)
import safe Data.Function (($), (.))
import safe Data.Functor (Functor((<$)), ($>), (<$>), fmap, void)
import safe Data.Int (Int)
import safe qualified Data.Map as Map
import safe Data.Maybe (Maybe(Just, Nothing), maybe)
import safe Data.Monoid (Monoid(mempty), (<>))
import safe Data.Text (Text)
import safe Data.Traversable (Traversable(traverse))
import safe Data.Tuple (fst)
import safe GHC.Err (error)

import safe qualified CMM.AST as AST
import safe CMM.AST.Annot (Annot, Annotation(Annot), updateAnnots, withAnnot, unAnnot)
import safe CMM.AST.BlockAnnot
  ( BlockAnnot(Begins, NoBlock, PartOf, Unreachable)
  , HasBlockAnnot(getBlockAnnot)
  , WithBlockAnnot(withBlockAnnot)
  )
import safe CMM.AST.Blockifier.Error
  ( BlockifierError(FlatteningInconsistency, GotoWithoutTargets,
                UnreachableStatement)
  , continuationFallthrough
  , duplicateSymbol
  )
import safe qualified CMM.AST.Blockifier.State as State
import safe CMM.AST.Blockifier.State (Blockifier, BlockifierState)
import safe CMM.AST.GetName (GetName(getName))
import safe CMM.AST.Maps (ASTmap(astMapM), Constraint, Space)
import safe CMM.AST.Utils
  ( GetTrivialGotoTarget(getTrivialGotoTarget)
  , getExprLVName
  )
import safe CMM.AST.Variables.SymbolType
  ( SymbolType(ConstDeclSymbol, ContSymbol, DatumLabelSymbol,
           ImportSymbol, LabelSymbol, RegisterSymbol)
  )
import safe CMM.FlowAnalysis (analyzeFlow)
import safe CMM.Parser.ASTError (registerASTError, registerASTWarning)
import safe CMM.Parser.HasPos (HasPos(getPos), SourcePos)
import safe CMM.Pretty ()
import safe CMM.Utils (addPrefix)
import safe Data.Map (Map)

helperName :: Text -> Text
helperName = addPrefix lrAnalysisPrefix

lrAnalysisPrefix :: Text
lrAnalysisPrefix = "LR"

blockIsSet :: Blockifier Bool
blockIsSet = uses State.currentBlock $ not . null

blocksCache :: Text -> Blockifier Int
blocksCache name = do
  table <- use State.blocksTable
  case name `Map.lookup` table of
    Just index -> return index
    Nothing ->
      let index = Map.size table
       in index <$ (State.blocksTable .= Map.insert name index table)

setCurrentBlock :: Text -> Blockifier Int
setCurrentBlock name = do
  index <- blocksCache name
  State.currentBlock ?= index
  return index

updateBlock :: Maybe Text -> Blockifier ()
updateBlock mName = do
  mIndex <- traverse blocksCache mName
  use State.currentBlock >>= \case
    Just oldName -> do
      cData <- use State.currentData
      State.blockData %= Map.insert oldName cData
      State.currentBlock .= mIndex
      State.currentData .= mempty
    Nothing -> State.currentBlock .= mIndex

setBlock :: Text -> Blockifier ()
setBlock = updateBlock . Just

unsetBlock :: Blockifier ()
unsetBlock = updateBlock Nothing

noBlockAnnots :: (Functor n, WithBlockAnnot a b) => n a -> n b
noBlockAnnots = updateAnnots (withBlockAnnot NoBlock)

withNoBlockAnnot :: WithBlockAnnot a b => a -> n b -> Annot n b
withNoBlockAnnot = withAnnot . withBlockAnnot NoBlock

addControlFlow :: Text -> Blockifier ()
addControlFlow destBlock =
  use State.currentBlock >>= \case
    Nothing -> pure ()
    Just block -> do
      index <- blocksCache destBlock
      State.controlFlow %= ((block, index) :)

class MetadataType t =>
      Register t n
  where
  register :: t -> n -> Blockifier ()

instance Register ReadsVars Text where
  register _ var =
    State.currentData %=
    Map.insertWith
      (\_ (reads, writes, lives) -> (not writes || reads, writes, lives))
      var
      (True, False, True)

instance Register WritesVars Text where
  register _ var =
    State.currentData %=
    Map.insertWith
      (\_ (reads, _, lives) -> (reads, True, lives))
      var
      (False, True, False)

instance (GetMetadata t (n a), Register t Text) => Register t (n a) where
  register t = traverse_ (register t) . getMetadata t

registerReads :: Register ReadsVars n => n -> Blockifier ()
registerReads = register ReadsVars

registerWrites :: Register WritesVars n => n -> Blockifier ()
registerWrites = register WritesVars

registerReadsWrites ::
     (Register WritesVars n, Register ReadsVars n) => n -> Blockifier ()
registerReadsWrites n = registerReads n *> registerWrites n

class NeverReturns n where
  neverReturns :: n -> Bool

instance NeverReturns (n a) => NeverReturns (Annot n a) where
  neverReturns (Annot n _) = neverReturns n

instance NeverReturns (n a) => NeverReturns [n a] where
  neverReturns = any neverReturns

instance NeverReturns (AST.CallAnnot a) where
  neverReturns =
    \case
      AST.FlowAnnot flow -> neverReturns flow
      AST.AliasAnnot {} -> False

instance NeverReturns (AST.Flow a) where
  neverReturns =
    \case
      AST.NeverReturns -> True
      _ -> False

class GetTargetNames n t | n -> t where
  getTargetNames :: n -> t

instance GetTargetNames (n a) b => GetTargetNames (Annot n a) b where
  getTargetNames (Annot n _) = getTargetNames n

instance GetTargetNames (AST.Body a) (Maybe [Text]) where
  getTargetNames =
    \case
      AST.Body [bodyItem] -> getTargetNames bodyItem
      _ -> error "Does not have targets"

instance GetTargetNames (AST.BodyItem a) (Maybe [Text]) where
  getTargetNames =
    \case
      AST.BodyStmt stmt -> getTargetNames stmt
      _ -> error "Does not have targets"

instance GetTargetNames (AST.Stmt a) (Maybe [Text]) where
  getTargetNames =
    \case
      AST.GotoStmt _ mTargets -> getTargetNames <$> mTargets
      AST.CallStmt _ _ _ _ mTargets _ -> getTargetNames <$> mTargets
      AST.JumpStmt _ _ _ mTargets -> getTargetNames <$> mTargets
      _ -> error "Does not have targets"

instance GetTargetNames (AST.Targets a) [Text] where
  getTargetNames (AST.Targets names) = getName <$> names

addBlockAnnot ::
     (HasPos a, WithBlockAnnot a b) => Annot AST.Stmt a -> Blockifier (Annot AST.Stmt b)
addBlockAnnot stmt@(Annot n annot) =
  use State.currentBlock >>= \case
    Nothing ->
      registerASTWarning stmt (UnreachableStatement . void $ unAnnot stmt) $>
      withAnnot (withBlockAnnot Unreachable annot) (noBlockAnnots n)
    Just block ->
      return . withAnnot (withBlockAnnot (PartOf block) annot) $ noBlockAnnots n

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

instance GetMetadata DeclaresVars (AST.Decl a) where
  getMetadata t =
    \case
      AST.RegDecl _ regs -> getMetadata t regs
      _ -> []

instance GetMetadata DeclaresVars (AST.Registers a) where
  getMetadata _ (AST.Registers _ _ nameStrLits) = getName . fst <$> nameStrLits

instance (GetMetadata t (AST.BodyItem a), MetadataType t) =>
         GetMetadata t (AST.Body a) where
  getMetadata t (AST.Body bodyItems) = getMetadata t bodyItems

instance GetMetadata ReadsVars (AST.BodyItem a) where
  getMetadata t =
    \case
      AST.BodyStmt stmt -> getMetadata t stmt
      _ -> []

instance GetMetadata WritesVars (AST.BodyItem a) where
  getMetadata t =
    \case
      AST.BodyStmt stmt -> getMetadata t stmt
      _ -> []

instance GetMetadata WritesVars (AST.Formal a) where
  getMetadata _ formal = [getName formal]

instance GetMetadata DeclaresVars (AST.BodyItem a) where
  getMetadata t =
    \case
      AST.BodyDecl stackDecl -> getMetadata t stackDecl
      AST.BodyStackDecl {} -> []
      AST.BodyStmt stmt -> getMetadata t stmt

instance GetMetadata ReadsVars (AST.Actual a) where
  getMetadata t (AST.Actual _ expr) = getMetadata t expr

instance GetMetadata ReadsVars (AST.Stmt a) where
  getMetadata t =
    \case
      AST.EmptyStmt -> []
      AST.IfStmt expr tBody eBody ->
        getMetadata t expr <> getMetadata t tBody <> getMetadata t eBody
      AST.SwitchStmt expr arms -> getMetadata t expr <> getMetadata t arms
      AST.SpanStmt key value body ->
        getMetadata t key <> getMetadata t value <> getMetadata t body
      AST.AssignStmt _ exprs -> getMetadata t exprs
      AST.PrimOpStmt _ _ actuals _ -> getMetadata t actuals
      AST.CallStmt _ _ expr actuals _ _ ->
        getMetadata t expr <> getMetadata t actuals
      AST.JumpStmt _ expr actuals _ -> getMetadata t expr <> getMetadata t actuals
      AST.ReturnStmt _ _ actuals -> getMetadata t actuals
      AST.LabelStmt {} -> []
      AST.ContStmt {} -> []
      AST.GotoStmt expr _ -> getMetadata t expr
      AST.CutToStmt expr actuals _ -> getMetadata t expr <> getMetadata t actuals

instance GetMetadata WritesVars (AST.Stmt a) where
  getMetadata t =
    \case
      AST.IfStmt _ tBody eBody -> getMetadata t tBody <> getMetadata t eBody
      AST.SwitchStmt _ arms -> getMetadata t arms
      AST.SpanStmt _ _ body -> getMetadata t body
      AST.AssignStmt lvalues _ -> getMetadata t lvalues
      AST.PrimOpStmt name _ _ _ -> [getName name]
      AST.CallStmt kindNames _ _ _ _ _ -> getMetadata t kindNames
      AST.ContStmt _ kindNames -> getMetadata t kindNames
      _ -> []

instance GetMetadata DeclaresVars (AST.Stmt a) where
  getMetadata t =
    \case
      AST.IfStmt _ tBody eBody -> getMetadata t tBody <> getMetadata t eBody
      AST.SwitchStmt _ arms -> getMetadata t arms
      AST.SpanStmt _ _ body -> getMetadata t body
      _ -> []

instance GetMetadata WritesVars (AST.KindName a) where
  getMetadata _ =
    \case
      kindName -> [getName kindName]

instance GetMetadata ReadsVars (AST.LValue a) where
  getMetadata t =
    \case
      AST.LVName name -> [getName name]
      AST.LVRef _ expr _ -> getMetadata t expr

instance GetMetadata WritesVars (AST.LValue a) where
  getMetadata _ =
    \case
      AST.LVName name -> [getName name]
      AST.LVRef {} -> []

instance GetMetadata ReadsVars (AST.Expr a) where
  getMetadata t =
    \case
      AST.LitExpr {} -> []
      AST.LVExpr lvalue -> getMetadata t lvalue
      AST.ParExpr expr -> getMetadata t expr
      AST.BinOpExpr _ left right -> getMetadata t left <> getMetadata t right
      AST.ComExpr expr -> getMetadata t expr
      AST.NegExpr expr -> getMetadata t expr
      AST.MemberExpr expr _ -> getMetadata t expr
      AST.InfixExpr _ left right -> getMetadata t left <> getMetadata t right
      AST.PrefixExpr _ actuals -> getMetadata t actuals

instance GetMetadata ReadsVars (AST.Arm a) where
  getMetadata t =
    \case
      AST.Arm ranges body -> getMetadata t ranges <> getMetadata t body

instance GetMetadata ReadsVars (AST.Range a) where
  getMetadata t =
    \case
      AST.Range left right -> getMetadata t left <> getMetadata t right

instance GetMetadata WritesVars (AST.Arm a) where
  getMetadata t =
    \case
      AST.Arm _ body -> getMetadata t body

instance GetMetadata DeclaresVars (AST.Arm a) where
  getMetadata t =
    \case
      AST.Arm _ body -> getMetadata t body

class Blockify n a b where
  blockify :: (WithBlockAnnot a b, HasPos a) => n a -> Blockifier (n b)

data BlockifyHint =
  BlockifyHint

type instance Constraint BlockifyHint a b =
     (WithBlockAnnot a b, HasPos a)

type instance Space BlockifyHint = Blockify'

class Blockify' a b n where
  blockify' :: (WithBlockAnnot a b, HasPos a) => n a -> Blockifier (n b)

instance Blockify (Annot n) a b => Blockify' a b (Annot n) where
  blockify' = blockify

instance Blockify' a b AST.Name where
  blockify' n = return $ withBlockAnnot NoBlock <$> n

instance {-# OVERLAPPABLE #-} ASTmap BlockifyHint n a b =>
                              Blockify (Annot n) a b where
  blockify (Annot n a) =
    withAnnot (withBlockAnnot NoBlock a) <$> astMapM BlockifyHint blockify' n

instance Blockify (Annot AST.Datum) a b where
  blockify datum@(datum' `Annot` _) =
    case datum' of
      AST.DatumLabel {} ->
        storeSymbol State.stackLabels DatumLabelSymbol datum $> noBlockAnnots datum
      _ -> return $ noBlockAnnots datum

blockifyProcedureHeader ::
     (HasPos a, WithBlockAnnot a b)
  => Annotation AST.ProcedureHeader a
  -> Blockifier (Annot AST.ProcedureHeader b)
blockifyProcedureHeader (AST.ProcedureHeader mConv name formals mType `Annot` a) = do
  formals' <- traverse blockify formals
  traverse_ registerWrites formals
  return . withNoBlockAnnot a $
    AST.ProcedureHeader
      mConv
      (noBlockAnnots name)
      formals'
      (fmap noBlockAnnots <$> mType)

instance Blockify (Annot AST.Procedure) a b where
  blockify procedure@(AST.Procedure header body `Annot` a) = do
    index <- setCurrentBlock $ helperName "procedure"
    header' <- blockifyProcedureHeader header
    withAnnot (Begins index `withBlockAnnot` a) <$>
      (AST.Procedure header' <$> blockify body) <*
      unsetBlock <*
      analyzeFlow procedure <*
      State.clearBlockifier

instance Blockify (Annot AST.ProcedureDecl) a b where
  blockify (AST.ProcedureDecl header `Annot` a) = do
    index <- setCurrentBlock $ helperName "procedure"
    header' <- blockifyProcedureHeader header
    withAnnot (Begins index `withBlockAnnot` a) (AST.ProcedureDecl header') <$
      unsetBlock <*
      State.clearBlockifier

instance Blockify (Annot AST.Body) a b where
  blockify (AST.Body bodyItems `Annot` a) =
    withNoBlockAnnot a . AST.Body <$> traverse blockify bodyItems

constructBlockified ::
     ( Blockify (Annot n1) a1 b1
     , WithBlockAnnot a1 b1
     , WithBlockAnnot a2 b2
     , HasPos a1
     )
  => (Annot n1 b1 -> n2 b2)
  -> a2
  -> Annot n1 a1
  -> Blockifier (Annot n2 b2)
constructBlockified constr a n = do
  n' <- blockify n
  return . withAnnot (withBlockAnnot (getBlockAnnot n') a) $ constr n'

instance Blockify (Annot AST.BodyItem) a b where
  blockify (item `Annot` a) =
    case item of
      AST.BodyStmt stmt -> constructBlockified AST.BodyStmt a stmt
      AST.BodyDecl decl -> constructBlockified AST.BodyDecl a decl
      AST.BodyStackDecl stackDecl -> constructBlockified AST.BodyStackDecl a stackDecl

instance Blockify (Annot AST.StackDecl) a b where
  blockify (AST.StackDecl datums `Annot` a) =
    withNoBlockAnnot a . AST.StackDecl <$> traverse blockify datums

instance Blockify (Annot AST.Decl) a b where
  blockify decl@(decl' `Annot` a) =
    case decl' of
      AST.RegDecl invar regs -> constructBlockified (AST.RegDecl invar) a regs
      AST.ImportDecl imports' ->
        withNoBlockAnnot a . AST.ImportDecl <$> traverse blockify imports'
      AST.ConstDecl {} ->
        storeSymbol State.constants ConstDeclSymbol decl $> noBlockAnnots decl
      _ -> return $ noBlockAnnots decl

instance Blockify (Annot AST.Import) a b where
  blockify import'@(Annot AST.Import {} _) =
    storeSymbol State.imports ImportSymbol import' $> noBlockAnnots import'

instance Blockify (Annot AST.Registers) a b where
  blockify regs@(AST.Registers _ _ nameStrLits `Annot` _) =
    traverse_ (storeRegister . fst) nameStrLits $> noBlockAnnots regs

instance Blockify (Annot AST.Formal) a b where
  blockify formal = storeRegister formal $> noBlockAnnots formal

storeRegister :: (GetName n, HasPos n) => n -> Blockifier ()
storeRegister = storeSymbol State.registers RegisterSymbol

storeSymbol ::
     (GetName n, HasPos n)
  => Lens BlockifierState BlockifierState (Map Text SourcePos) (Map Text SourcePos)
  -> SymbolType
  -> n
  -> Blockifier ()
storeSymbol symbolMap symbolName node = do
  symbols' <- use symbolMap
  if getName node `Map.member` symbols'
    then registerASTError node $ duplicateSymbol symbolName node
    else symbolMap .= Map.insert (getName node) (getPos node) symbols'

instance Blockify (Annot AST.Stmt) a b where
  blockify stmt@(stmt' `Annot` a) =
    case stmt' of
      AST.LabelStmt {} -> do
        addControlFlow $ getName stmt -- a possible fallthrough
        storeSymbol State.labels LabelSymbol stmt
        blockifyLabelStmt stmt
      AST.ContStmt {} -> do
        storeSymbol State.continuations ContSymbol stmt
        blockIsSet >>=
          (`when` registerASTError stmt (continuationFallthrough stmt))
        blockifyLabelStmt stmt <* registerWrites stmt
      AST.GotoStmt expr _ -> do
        case (getExprLVName expr, getTargetNames stmt) of
          (Nothing, Just targets@(_:_)) -> traverse_ addControlFlow targets
          (Just name, Just targets@(_:_)) ->
            if name `elem` targets
              then addControlFlow name
              else traverse_ addControlFlow targets
          (Just name, _) -> addControlFlow name
          (Nothing, _) -> registerASTError stmt . GotoWithoutTargets . void $ unAnnot stmt
        registerReads stmt *> addBlockAnnot stmt <* unsetBlock
      AST.CutToStmt {} ->
        error "'Cut to' statements are not currently implemented"
      AST.ReturnStmt {} -> registerReads stmt *> addBlockAnnot stmt <* unsetBlock
      AST.JumpStmt {} -> registerReads stmt *> addBlockAnnot stmt <* unsetBlock
      AST.EmptyStmt {} ->
        addBlockAnnot stmt -- This should be completely redundant, included just for completeness
      AST.AssignStmt {} -> registerReadsWrites stmt *> addBlockAnnot stmt
      AST.PrimOpStmt {} -- NOTE: In the future, this may end a basic block if given `NeverReturns` flow annotation
       -> registerReadsWrites stmt *> addBlockAnnot stmt
      AST.IfStmt _ tBody mEBody -> do
        case (getTrivialGotoTarget tBody, getTrivialGotoTarget <$> mEBody) of
          (Just left, Just (Just right)) -> do
            addControlFlow left
            addControlFlow right
          (Just left, Nothing) -> do
            addControlFlow left
          _ -> flatteningError stmt
        addBlockAnnot stmt <* unsetBlock
      AST.SwitchStmt _ arms -> do
        case traverse getTrivialGotoTarget arms of
          Just names -> traverse_ addControlFlow names
          Nothing -> flatteningError stmt
        addBlockAnnot stmt <* unsetBlock
      AST.SpanStmt key value body ->
        withNoBlockAnnot a . AST.SpanStmt (noBlockAnnots key) (noBlockAnnots value) <$>
        blockify body
      AST.CallStmt _ _ _ _ _ callAnnots -- NOTE: `cut to` statements not supported
       ->
        registerReadsWrites stmt *> addBlockAnnot stmt <*
        when (neverReturns callAnnots) unsetBlock

-- This is here just for completeness
flatteningError :: HasPos (Annot AST.Stmt a) => Annot AST.Stmt a -> Blockifier ()
flatteningError stmt = registerASTError stmt . FlatteningInconsistency . void $ unAnnot stmt

blockifyLabelStmt ::
     WithBlockAnnot a b => Annot AST.Stmt a -> Blockifier (Annot AST.Stmt b)
blockifyLabelStmt (Annot stmt a) = do
  let name = getName stmt
  setBlock name
  index <- blocksCache name
  return . withAnnot (withBlockAnnot (Begins index) a) $ noBlockAnnots stmt
