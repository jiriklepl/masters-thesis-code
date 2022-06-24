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

import safe CMM.AST
  ( Actual(Actual)
  , Arm(Arm)
  , Body(Body)
  , BodyItem(BodyDecl, BodyStackDecl, BodyStmt)
  , CallAnnot(AliasAnnot, FlowAnnot)
  , Datum(DatumLabel)
  , Decl(ConstDecl, ImportDecl, RegDecl)
  , Expr(BinOpExpr, ComExpr, InfixExpr, LVExpr, LitExpr, MemberExpr,
     NegExpr, ParExpr, PrefixExpr)
  , Flow(NeverReturns)
  , Formal
  , Import(Import)
  , KindName
  , LValue(LVName, LVRef)
  , Name
  , Procedure(Procedure)
  , ProcedureDecl(ProcedureDecl)
  , ProcedureHeader(ProcedureHeader)
  , Range(Range)
  , Registers(Registers)
  , StackDecl(StackDecl)
  , Stmt(AssignStmt, CallStmt, ContStmt, CutToStmt, EmptyStmt,
     GotoStmt, IfStmt, JumpStmt, LabelStmt, PrimOpStmt, ReturnStmt,
     SpanStmt, SwitchStmt)
  , Targets(Targets)
  )
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
import safe CMM.AST.Blockifier.State
  ( Blockifier
  , BlockifierState
  , blockData
  , blocksTable
  , clearBlockifier
  , constants
  , continuations
  , controlFlow
  , currentBlock
  , currentData
  , imports
  , labels
  , registers
  , stackLabels
  )
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
blockIsSet = uses currentBlock $ not . null

blocksCache :: Text -> Blockifier Int
blocksCache name = do
  table <- use blocksTable
  case name `Map.lookup` table of
    Just index -> return index
    Nothing ->
      let index = Map.size table
       in index <$ (blocksTable .= Map.insert name index table)

setCurrentBlock :: Text -> Blockifier Int
setCurrentBlock name = do
  index <- blocksCache name
  currentBlock ?= index
  return index

updateBlock :: Maybe Text -> Blockifier ()
updateBlock mName = do
  mIndex <- traverse blocksCache mName
  use currentBlock >>= \case
    Just oldName -> do
      cData <- use currentData
      blockData %= Map.insert oldName cData
      currentBlock .= mIndex
      currentData .= mempty
    Nothing -> currentBlock .= mIndex

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
  use currentBlock >>= \case
    Nothing -> pure ()
    Just block -> do
      index <- blocksCache destBlock
      controlFlow %= ((block, index) :)

class MetadataType t =>
      Register t n
  where
  register :: t -> n -> Blockifier ()

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

instance NeverReturns (CallAnnot a) where
  neverReturns =
    \case
      FlowAnnot flow -> neverReturns flow
      AliasAnnot {} -> False

instance NeverReturns (Flow a) where
  neverReturns =
    \case
      NeverReturns -> True
      _ -> False

class GetTargetNames n t | n -> t where
  getTargetNames :: n -> t

instance GetTargetNames (n a) b => GetTargetNames (Annot n a) b where
  getTargetNames (Annot n _) = getTargetNames n

instance GetTargetNames (Body a) (Maybe [Text]) where
  getTargetNames =
    \case
      Body [bodyItem] -> getTargetNames bodyItem
      _ -> error "Does not have targets"

instance GetTargetNames (BodyItem a) (Maybe [Text]) where
  getTargetNames =
    \case
      BodyStmt stmt -> getTargetNames stmt
      _ -> error "Does not have targets"

instance GetTargetNames (Stmt a) (Maybe [Text]) where
  getTargetNames =
    \case
      GotoStmt _ mTargets -> getTargetNames <$> mTargets
      CallStmt _ _ _ _ mTargets _ -> getTargetNames <$> mTargets
      JumpStmt _ _ _ mTargets -> getTargetNames <$> mTargets
      _ -> error "Does not have targets"

instance GetTargetNames (Targets a) [Text] where
  getTargetNames (Targets names) = getName <$> names

addBlockAnnot ::
     (HasPos a, WithBlockAnnot a b) => Annot Stmt a -> Blockifier (Annot Stmt b)
addBlockAnnot stmt@(Annot n annot) =
  use currentBlock >>= \case
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

instance GetMetadata DeclaresVars (Decl a) where
  getMetadata t =
    \case
      RegDecl _ regs -> getMetadata t regs
      _ -> []

instance GetMetadata DeclaresVars (Registers a) where
  getMetadata _ (Registers _ _ nameStrLits) = getName . fst <$> nameStrLits

instance (GetMetadata t (BodyItem a), MetadataType t) =>
         GetMetadata t (Body a) where
  getMetadata t (Body bodyItems) = getMetadata t bodyItems

instance GetMetadata ReadsVars (BodyItem a) where
  getMetadata t =
    \case
      BodyStmt stmt -> getMetadata t stmt
      _ -> []

instance GetMetadata WritesVars (BodyItem a) where
  getMetadata t =
    \case
      BodyStmt stmt -> getMetadata t stmt
      _ -> []

instance GetMetadata WritesVars (Formal a) where
  getMetadata _ formal = [getName formal]

instance GetMetadata DeclaresVars (BodyItem a) where
  getMetadata t =
    \case
      BodyDecl stackDecl -> getMetadata t stackDecl
      BodyStackDecl {} -> []
      BodyStmt stmt -> getMetadata t stmt

instance GetMetadata ReadsVars (Actual a) where
  getMetadata t (Actual _ expr) = getMetadata t expr

instance GetMetadata ReadsVars (Stmt a) where
  getMetadata t =
    \case
      EmptyStmt -> []
      IfStmt expr tBody eBody ->
        getMetadata t expr <> getMetadata t tBody <> getMetadata t eBody
      SwitchStmt expr arms -> getMetadata t expr <> getMetadata t arms
      SpanStmt key value body ->
        getMetadata t key <> getMetadata t value <> getMetadata t body
      AssignStmt _ exprs -> getMetadata t exprs
      PrimOpStmt _ _ actuals _ -> getMetadata t actuals
      CallStmt _ _ expr actuals _ _ ->
        getMetadata t expr <> getMetadata t actuals
      JumpStmt _ expr actuals _ -> getMetadata t expr <> getMetadata t actuals
      ReturnStmt _ _ actuals -> getMetadata t actuals
      LabelStmt {} -> []
      ContStmt {} -> []
      GotoStmt expr _ -> getMetadata t expr
      CutToStmt expr actuals _ -> getMetadata t expr <> getMetadata t actuals

instance GetMetadata WritesVars (Stmt a) where
  getMetadata t =
    \case
      IfStmt _ tBody eBody -> getMetadata t tBody <> getMetadata t eBody
      SwitchStmt _ arms -> getMetadata t arms
      SpanStmt _ _ body -> getMetadata t body
      AssignStmt lvalues _ -> getMetadata t lvalues
      PrimOpStmt name _ _ _ -> [getName name]
      CallStmt kindNames _ _ _ _ _ -> getMetadata t kindNames
      ContStmt _ kindNames -> getMetadata t kindNames
      _ -> []

instance GetMetadata DeclaresVars (Stmt a) where
  getMetadata t =
    \case
      IfStmt _ tBody eBody -> getMetadata t tBody <> getMetadata t eBody
      SwitchStmt _ arms -> getMetadata t arms
      SpanStmt _ _ body -> getMetadata t body
      _ -> []

instance GetMetadata WritesVars (KindName a) where
  getMetadata _ =
    \case
      kindName -> [getName kindName]

instance GetMetadata ReadsVars (LValue a) where
  getMetadata t =
    \case
      LVName name -> [getName name]
      LVRef _ expr _ -> getMetadata t expr

instance GetMetadata WritesVars (LValue a) where
  getMetadata _ =
    \case
      LVName name -> [getName name]
      LVRef {} -> []

instance GetMetadata ReadsVars (Expr a) where
  getMetadata t =
    \case
      LitExpr {} -> []
      LVExpr lvalue -> getMetadata t lvalue
      ParExpr expr -> getMetadata t expr
      BinOpExpr _ left right -> getMetadata t left <> getMetadata t right
      ComExpr expr -> getMetadata t expr
      NegExpr expr -> getMetadata t expr
      MemberExpr expr _ -> getMetadata t expr -- TODO: check whether this is correct
      InfixExpr _ left right -> getMetadata t left <> getMetadata t right
      PrefixExpr _ actuals -> getMetadata t actuals

instance GetMetadata ReadsVars (Arm a) where
  getMetadata t =
    \case
      Arm ranges body -> getMetadata t ranges <> getMetadata t body

instance GetMetadata ReadsVars (Range a) where
  getMetadata t =
    \case
      Range left right -> getMetadata t left <> getMetadata t right

instance GetMetadata WritesVars (Arm a) where
  getMetadata t =
    \case
      Arm _ body -> getMetadata t body

instance GetMetadata DeclaresVars (Arm a) where
  getMetadata t =
    \case
      Arm _ body -> getMetadata t body

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

instance Blockify' a b Name where
  blockify' n = return $ withBlockAnnot NoBlock <$> n

instance {-# OVERLAPPABLE #-} ASTmap BlockifyHint n a b =>
                              Blockify (Annot n) a b where
  blockify (Annot n a) =
    withAnnot (withBlockAnnot NoBlock a) <$> astMapM BlockifyHint blockify' n

instance Blockify (Annot Datum) a b where
  blockify datum@(datum' `Annot` _) =
    case datum' of
      DatumLabel {} ->
        storeSymbol stackLabels DatumLabelSymbol datum $> noBlockAnnots datum
      _ -> return $ noBlockAnnots datum

blockifyProcedureHeader ::
     (HasPos a, WithBlockAnnot a b)
  => Annotation ProcedureHeader a
  -> Blockifier (Annot ProcedureHeader b)
blockifyProcedureHeader (ProcedureHeader mConv name formals mType `Annot` a) = do
  formals' <- traverse blockify formals
  traverse_ registerWrites formals
  return . withNoBlockAnnot a $
    ProcedureHeader
      mConv
      (noBlockAnnots name)
      formals'
      (fmap noBlockAnnots <$> mType)

instance Blockify (Annot Procedure) a b where
  blockify procedure@(Procedure header body `Annot` a) = do
    index <- setCurrentBlock $ helperName "procedure"
    header' <- blockifyProcedureHeader header
    withAnnot (Begins index `withBlockAnnot` a) <$>
      (Procedure header' <$> blockify body) <*
      unsetBlock <*
      analyzeFlow procedure <*
      clearBlockifier

instance Blockify (Annot ProcedureDecl) a b where
  blockify (ProcedureDecl header `Annot` a) = do
    index <- setCurrentBlock $ helperName "procedure"
    header' <- blockifyProcedureHeader header
    withAnnot (Begins index `withBlockAnnot` a) (ProcedureDecl header') <$
      unsetBlock <*
      clearBlockifier

instance Blockify (Annot Body) a b where
  blockify (Body bodyItems `Annot` a) =
    withNoBlockAnnot a . Body <$> traverse blockify bodyItems

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

instance Blockify (Annot BodyItem) a b where
  blockify (item `Annot` a) =
    case item of
      BodyStmt stmt -> constructBlockified BodyStmt a stmt
      BodyDecl decl -> constructBlockified BodyDecl a decl
      BodyStackDecl stackDecl -> constructBlockified BodyStackDecl a stackDecl

instance Blockify (Annot StackDecl) a b where
  blockify (StackDecl datums `Annot` a) =
    withNoBlockAnnot a . StackDecl <$> traverse blockify datums

instance Blockify (Annot Decl) a b where
  blockify decl@(decl' `Annot` a) =
    case decl' of
      RegDecl invar regs -> constructBlockified (RegDecl invar) a regs
      ImportDecl imports' ->
        withNoBlockAnnot a . ImportDecl <$> traverse blockify imports'
      ConstDecl {} ->
        storeSymbol constants ConstDeclSymbol decl $> noBlockAnnots decl
      _ -> return $ noBlockAnnots decl

instance Blockify (Annot Import) a b where
  blockify import'@(Annot Import {} _) =
    storeSymbol imports ImportSymbol import' $> noBlockAnnots import'

instance Blockify (Annot Registers) a b where
  blockify regs@(Registers _ _ nameStrLits `Annot` _) =
    traverse_ (storeRegister . fst) nameStrLits $> noBlockAnnots regs

instance Blockify (Annot Formal) a b where
  blockify formal = storeRegister formal $> noBlockAnnots formal

storeRegister :: (GetName n, HasPos n) => n -> Blockifier ()
storeRegister = storeSymbol registers RegisterSymbol

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

instance Blockify (Annot Stmt) a b where
  blockify stmt@(stmt' `Annot` a) =
    case stmt' of
      LabelStmt {} -> do
        addControlFlow $ getName stmt -- a possible fallthrough
        storeSymbol labels LabelSymbol stmt
        blockifyLabelStmt stmt
      ContStmt {} -> do
        storeSymbol continuations ContSymbol stmt
        blockIsSet >>=
          (`when` registerASTError stmt (continuationFallthrough stmt))
        blockifyLabelStmt stmt <* registerWrites stmt
      GotoStmt expr _ -> do
        case (getExprLVName expr, getTargetNames stmt) of
          (Nothing, Just targets@(_:_)) -> traverse_ addControlFlow targets
          (Just name, Just targets@(_:_)) ->
            if name `elem` targets
              then addControlFlow name
              else traverse_ addControlFlow targets
          (Just name, _) -> addControlFlow name
          (Nothing, _) -> registerASTError stmt . GotoWithoutTargets . void $ unAnnot stmt
        registerReads stmt *> addBlockAnnot stmt <* unsetBlock
      CutToStmt {} ->
        error "'Cut to' statements are not currently implemented" -- TODO: implement `cut to` statements
      ReturnStmt {} -> registerReads stmt *> addBlockAnnot stmt <* unsetBlock
      JumpStmt {} -> registerReads stmt *> addBlockAnnot stmt <* unsetBlock
      EmptyStmt {} ->
        addBlockAnnot stmt -- This should be completely redundant, included just for completeness
      AssignStmt {} -> registerReadsWrites stmt *> addBlockAnnot stmt
      PrimOpStmt {} -- FIXME: In the future, this may end a basic block if given `NeverReturns` flow annotation
       -> registerReadsWrites stmt *> addBlockAnnot stmt
      IfStmt _ tBody mEBody -> do
        case (getTrivialGotoTarget tBody, getTrivialGotoTarget <$> mEBody) of
          (Just left, Just (Just right)) -> do
            addControlFlow left
            addControlFlow right
          (Just left, Nothing) -> do
            addControlFlow left
          _ -> flatteningError stmt
        addBlockAnnot stmt <* unsetBlock
      SwitchStmt _ arms -> do
        case traverse getTrivialGotoTarget arms of
          Just names -> traverse_ addControlFlow names
          Nothing -> flatteningError stmt
        addBlockAnnot stmt <* unsetBlock
      SpanStmt key value body ->
        withNoBlockAnnot a . SpanStmt (noBlockAnnots key) (noBlockAnnots value) <$>
        blockify body
      CallStmt _ _ _ _ _ callAnnots -- TODO: implement `cut to` statements
       ->
        registerReadsWrites stmt *> addBlockAnnot stmt <*
        when (neverReturns callAnnots) unsetBlock

-- This is here just for completeness
flatteningError :: HasPos (Annot Stmt a) => Annot Stmt a -> Blockifier ()
flatteningError stmt = registerASTError stmt . FlatteningInconsistency . void $ unAnnot stmt

blockifyLabelStmt ::
     WithBlockAnnot a b => Annot Stmt a -> Blockifier (Annot Stmt b)
blockifyLabelStmt (Annot stmt a) = do
  let name = getName stmt
  setBlock name
  index <- blocksCache name -- TODO: this is not optimal
  return . withAnnot (withBlockAnnot (Begins index) a) $ noBlockAnnots stmt
