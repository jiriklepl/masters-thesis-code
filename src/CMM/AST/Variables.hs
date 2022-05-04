{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}

module CMM.AST.Variables where

import safe Control.Applicative (Applicative((*>), (<*)))
import safe Control.Monad (Monad(return))
import safe Control.Monad.State (execState)
import safe Data.Data (Data(gmapM))
import safe Data.Foldable (Foldable(foldr), traverse_)
import safe Data.Function (($), (.))
import safe Data.Functor (Functor((<$)), (<$>))
import safe Data.Maybe (Maybe(Just, Nothing))
import safe qualified Data.Set as Set
import safe Data.Tuple (fst)

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
import safe CMM.AST.GetName (GetName, getName)
import safe CMM.AST.Variables.State
  ( Collector
  , CollectorState
  , addFIVar
  , addFVar
  , addFVarTrivial
  , addSMemTrivial
  , addTAlias
  , addTClass
  , addTCon
  , addTVar
  , addTVarTrivial
  , addVar
  , addVarTrivial
  , initCollector
  )
import safe CMM.Inference.TypeKind
  ( TypeKind((:->), Constraint, GenericType, Star)
  )
import safe CMM.Parser.HasPos (HasPos, SourcePos, getPos)
import safe CMM.Data.Generics ( (<*|*>) )

localVariables ::
     (Data (n SourcePos), Functor n, HasPos a) => n a -> CollectorState
localVariables n = variablesCommon . go $ getPos <$> n
  where
    go :: Data d => d -> Collector d
    go = addTAutoCases $ addCommonCases $ gmapM go

globalVariables :: HasPos a => Unit a -> CollectorState
globalVariables n = variablesCommon . go $ getPos <$> n
  where
    go :: Data d => d -> Collector d
    go = addGlobalCases $ addCommonCases $ gmapM go

addParaNameTVars ::
     (HasPos annot, GetName (param annot))
  => Annot (ParaName param) annot
  -> Collector ()
addParaNameTVars (Annot (ParaName _ params) _) =
  traverse_ (`addTVarTrivial` GenericType) params

structVariables :: HasPos a => Struct a -> CollectorState
structVariables (Struct paraName datums) =
  variablesCommon $ do
    traverse_
      (`addSMemTrivial` Star)
      [label | label@(Annot DatumLabel {} _) <- datums]
    addParaNameTVars paraName

classVariables :: HasPos a => Class a -> CollectorState
classVariables class'@(Class _ paraName _) =
  variablesCommon $ do
    addParaNameTVars paraName
    gmapM go $ getPos <$> class'
  where
    go :: Data d => d -> Collector d
    go = addProcedureDeclCases $ gmapM go

instanceVariables :: HasPos a => Instance a -> CollectorState
instanceVariables n = variablesCommon . go $ getPos <$> n
  where
    go :: Data d => d -> Collector d
    go = addTAutoCases $ addProcedureCases $ gmapM go

variablesCommon :: Collector a -> CollectorState
variablesCommon = (`execState` initCollector)

type CasesAdder m a
   = Data a =>
       (forall d. Data d =>
                    d -> Collector d) -> a -> Collector a

addCommonCases :: CasesAdder m a
addCommonCases go =
  goFormal <*|*> goDecl <*|*> goImport <*|*> goRegisters <*|*> goDatum <*|*> goStmt <*|*> go
  where
    goFormal =
      \case
        (formal :: Annot Formal SourcePos) ->
          gmapM go formal *> addVarTrivial formal Star
    goDecl =
      \case
        decl@(Annot ConstDecl {} (_ :: SourcePos)) -> addVarTrivial decl Star
        decl@(Annot (TypedefDecl _ names) (_ :: SourcePos)) ->
          decl <$ traverse_ (`addTAlias` GenericType) names
        decl -> gmapM go decl
    goImport =
      \case
        (import' :: Annot Import SourcePos) -> addVarTrivial import' Star
    goRegisters =
      \case
        registers@(Annot (Registers _ _ nameStrLits) (_ :: SourcePos)) ->
          registers <$ traverse_ (`addVar` Star) (fst <$> nameStrLits)
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
  goClass <*|*> goInstance <*|*> goStruct <*|*> goSection <*|*> goProcedure <*|*> go
  where
    goClass =
      \case
        class'@(Annot (Class _ (Annot (ParaName _ args) _) methods) (_ :: SourcePos)) -> do
          traverse_ addMethod methods
          class' <$
            addTClass
              class'
              (foldr (:->) Constraint (Star <$ args))
              (Set.fromList $ getName <$> methods) -- TODO: add less trivial kind analysis (should be a simple bunch of unifs)
      where
        addMethod node = do
          addFVar node Star
          addFIVar node Star
    goInstance =
      \case
        (instance' :: Annot Instance SourcePos) -> return instance'
    goStruct =
      \case
        struct@(Annot (Struct (Annot (ParaName _ args) _) decls) (_ :: SourcePos)) -> do
          addTCon struct (foldr (:->) Star (Star <$ args)) -- TODO: add less trivial kind analysis
          traverse_
            (`addSMemTrivial` Star)
            [label | label@(Annot DatumLabel {} _) <- decls]
          return struct
    goSection =
      \case
        (section :: Annot Section SourcePos) -> gmapM goSectionItems section
    goProcedure =
      \case
        (procedure :: Annot Procedure SourcePos) ->
          addFVarTrivial procedure Star
    goSectionItems :: Data d => d -> Collector d
    goSectionItems = addSectionCases $ addCommonCases $ gmapM goSectionItems

addSectionCases :: CasesAdder m a
addSectionCases go = goProcedure <*|*> go
  where
    goProcedure =
      \case
        (procedure :: Annot Procedure SourcePos) ->
          addFVarTrivial procedure Star <* gmapM goLabels procedure
    goLabels :: Data d => d -> Collector d
    goLabels = addLabelCases $ gmapM goLabels

addProcedureDeclCases :: CasesAdder m a
addProcedureDeclCases go = goProcedureDecl <*|*> go
  where
    goProcedureDecl =
      \case
        (procedureDecl :: Annot ProcedureDecl SourcePos) ->
          addFVarTrivial procedureDecl Star

addProcedureCases :: CasesAdder m a
addProcedureCases go = goProcedure <*|*> go
  where
    goProcedure =
      \case
        (procedureDecl :: Annot Procedure SourcePos) ->
          addFVarTrivial procedureDecl Star

addLabelCases :: CasesAdder m a
addLabelCases go = goStmt <*|*> goDatum <*|*> go
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
addTAutoCases go = goTAuto <*|*> go
  where
    goTAuto =
      \case
        tAuto@(Annot (TAuto Nothing) (_ :: SourcePos)) -> return tAuto
        tAuto@(Annot (TAuto Just {}) (_ :: SourcePos)) ->
          tAuto <$ addTVar tAuto GenericType
        type' -> gmapM go type'
