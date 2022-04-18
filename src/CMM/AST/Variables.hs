{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}

module CMM.AST.Variables where

import safe Data.Functor ( Functor((<$)), (<$>) )
import safe Data.Maybe ( Maybe(Just, Nothing) )
import safe Data.Foldable ( Foldable(foldr), traverse_ )
import safe Data.List ( repeat )
import safe Data.Function ( ($), (.), flip )
import safe Data.Tuple ( fst )
import safe Control.Applicative ( Applicative((<*), (*>)) )
import safe Control.Lens.Getter ((^.))
import safe Control.Monad ( Monad(return), zipWithM_)
import safe Control.Monad.State (MonadIO, StateT, execStateT)
import safe Data.Data (Data(gmapM), Typeable)
import safe Data.Generics.Aliases (extM)
import safe Data.Map (Map)
import safe qualified Data.Set as Set
import safe Data.Set (Set)
import safe Data.Text (Text)

import safe CMM.AST
  ( Class(Class)
  , Datum(DatumLabel)
  , Decl(ConstDecl, TypedefDecl)
  , Formal
  , Import
  , Instance
  , ParaName(ParaName)
  , Procedure
  , ProcedureDecl
  , Registers(Registers)
  , Section
  , Stmt(LabelStmt)
  , Struct(Struct)
  , Type(TAuto)
  , Unit
  )
import safe CMM.AST.Annot (Annot, Annotation(Annot))
import safe CMM.AST.HasName (getName)
import safe CMM.AST.Variables.State
  ( Collector
  , MonadCollectVariables
  , addFIVar
  , addFVar
  , addFVarTrivial
  , addSMemTrivial
  , addTClass
  , addTCon
  , addTVar
  , addTVarTrivial
  , addVar
  , addVarTrivial
  , funcInstVariables
  , funcVariables
  , initCollector
  , structMembers
  , typeClasses
  , typeConstants
  , typeVariables
  , variables
  )
import safe CMM.Inference.TypeKind (TypeKind(..))
import safe CMM.Parser.HasPos (HasPos(..), SourcePos)

type VariablePack
   = ( Map Text (SourcePos, TypeKind)
     , Map Text (SourcePos, TypeKind)
     , Map Text (SourcePos, TypeKind)
     , Map Text (SourcePos, TypeKind)
     , Map Text (SourcePos, TypeKind)
     , Map Text (SourcePos, TypeKind, Set Text)
     , Map Text (SourcePos, TypeKind))

localVariables ::
     (MonadIO m, Data (n SourcePos), Functor n, HasPos a)
  => n a
  -> m VariablePack
localVariables n = variablesCommon . go $ getPos <$> n
  where
    go :: (Data d, MonadCollectVariables m) => d -> m d
    go = addTAutoCases $ addCommonCases $ gmapM go

globalVariables :: (MonadIO m, HasPos a) => Unit a -> m VariablePack
globalVariables n = variablesCommon . go $ getPos <$> n
  where
    go :: (Data d, MonadCollectVariables m) => d -> m d
    go = addGlobalCases $ addCommonCases $ gmapM go

classVariables :: (MonadIO m, HasPos a) => Class a -> m VariablePack
classVariables class'@(Class _ (Annot (ParaName _ params) _) _) =
  variablesCommon $ do
    traverse_ (`addTVarTrivial` GenericType) params
    gmapM go $ getPos <$> class'
  where
    go :: (Data d, MonadCollectVariables m) => d -> m d
    go = addProcedureDeclCases $ gmapM go

instanceVariables :: (MonadIO m, HasPos a) => Instance a -> m VariablePack
instanceVariables n = variablesCommon . go $ getPos <$> n
  where
    go :: (Data d, MonadCollectVariables m) => d -> m d
    go = addTAutoCases $ addProcedureCases $ gmapM go

variablesCommon :: MonadIO m => StateT Collector m a -> m VariablePack
variablesCommon go = do
  result <- execStateT go initCollector
  return
    ( result ^. variables
    , result ^. funcVariables
    , result ^. funcInstVariables
    , result ^. typeConstants
    , result ^. typeVariables
    , result ^. typeClasses
    , result ^. structMembers)

infixr 3 *|*

-- | An alias of flipped `extM`. Its behavior resembles that of the `<|>` method of `Alternative`, including the evaluation order (but mind the infixr fixity).
(*|*) ::
     (Monad m, Typeable a, Typeable b) => (b -> m b) -> (a -> m a) -> a -> m a
(*|*) = flip extM

type CasesAdder m a
   = (Data a, MonadCollectVariables m) =>
       (forall d. Data d =>
                    d -> m d) -> a -> m a

addCommonCases :: CasesAdder m a
addCommonCases go =
  goFormal *|* goDecl *|* goImport *|* goRegisters *|* goDatum *|* goStmt *|* go
  where
    goFormal =
      \case
        (formal :: Annot Formal SourcePos) ->
          gmapM go formal *> addVarTrivial formal Star
    goDecl =
      \case
        decl@(Annot ConstDecl {} (_ :: SourcePos)) -> addVarTrivial decl Star
        decl@(Annot (TypedefDecl _ names) (_ :: SourcePos)) ->
          decl <$
          traverse_ (flip (addTCon decl) GenericType) (getName <$> names)
        decl -> gmapM go decl
    goImport =
      \case
        (import' :: Annot Import SourcePos) -> addVarTrivial import' Star
    goRegisters =
      \case
        registers@(Annot (Registers _ _ nameStrLits) (_ :: SourcePos)) ->
          registers <$
          traverse_
            (flip (addVar registers) Star)
            (getName . fst <$> nameStrLits)
    goDatum =
      \case
        datum@(Annot DatumLabel {} (_ :: SourcePos)) -> addVarTrivial datum Star
        datum -> gmapM go datum
    goStmt =
      \case
        stmt@(Annot LabelStmt {} (_ :: SourcePos)) -> addVarTrivial stmt Star
        stmt -> gmapM go stmt

addGlobalCases :: CasesAdder m a
addGlobalCases go =
  goClass *|* goInstance *|* goStruct *|* goSection *|* goProcedure *|* go
  where
    goClass =
      \case
        class'@(Annot (Class _ (Annot (ParaName _ args) _) methods) (_ :: SourcePos)) -> do
          zipWithM_ addMethod methods (getName <$> methods)
          class' <$
            addTClass
              class'
              (getName class')
              (foldr (:->) Constraint (Star <$ args))
              (Set.fromList $ getName <$> methods) -- TODO: add less trivial kind analysis (should be a simple bunch of unifs)
      where
        addMethod node name = do
          addFVar node name Star
          addFIVar node name Star
    goInstance =
      \case
        (instance' :: Annot Instance SourcePos) -> return instance'
    goStruct =
      \case
        struct@(Annot (Struct (Annot (ParaName _ args) _) decls) (_ :: SourcePos)) -> do
          addTCon struct (getName struct) (foldr (:->) Star (Star <$ args)) -- TODO: add less trivial kind analysis
          zipWithM_
            addSMemTrivial
            [label | label@(Annot DatumLabel {} _) <- decls]
            (repeat Star)
          return struct
    goSection =
      \case
        (section :: Annot Section SourcePos) -> gmapM goSectionItems section
    goProcedure =
      \case
        (procedure :: Annot Procedure SourcePos) ->
          addFVarTrivial procedure Star
    goSectionItems :: (Data d, MonadCollectVariables m) => d -> m d
    goSectionItems = addSectionCases $ addCommonCases $ gmapM goSectionItems

addSectionCases :: CasesAdder m a
addSectionCases go = goProcedure *|* go
  where
    goProcedure =
      \case
        (procedure :: Annot Procedure SourcePos) ->
          addFVarTrivial procedure Star <* gmapM goLabels procedure
    goLabels :: (Data d, MonadCollectVariables m) => d -> m d
    goLabels = addLabelCases $ gmapM goLabels

addProcedureDeclCases :: CasesAdder m a
addProcedureDeclCases go = goProcedureDecl *|* go
  where
    goProcedureDecl =
      \case
        (procedureDecl :: Annot ProcedureDecl SourcePos) ->
          addFVarTrivial procedureDecl Star

addProcedureCases :: CasesAdder m a
addProcedureCases go = goProcedure *|* go
  where
    goProcedure =
      \case
        (procedureDecl :: Annot Procedure SourcePos) ->
          addFVarTrivial procedureDecl Star

addLabelCases :: CasesAdder m a
addLabelCases go = goStmt *|* goDatum *|* go
  where
    goStmt =
      \case
        stmt@(Annot LabelStmt {} (_ :: SourcePos)) -> addVarTrivial stmt Star
        stmt -> gmapM go stmt
    goDatum =
      \case
        datum@(Annot DatumLabel {} (_ :: SourcePos)) -> addVarTrivial datum Star
        datum -> gmapM go datum

addTAutoCases :: CasesAdder m a
addTAutoCases go = goTAuto *|* go
  where
    goTAuto =
      \case
        tAuto@(Annot (TAuto Nothing) (_ :: SourcePos)) -> return tAuto
        tAuto@(Annot (TAuto (Just n)) (_ :: SourcePos)) ->
          tAuto <$ addTVar tAuto (getName n) GenericType
        type' -> gmapM go type'
