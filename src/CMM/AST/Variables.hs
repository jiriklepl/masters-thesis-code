{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}

module CMM.AST.Variables where

import safe Control.Monad.State (execState)
import safe Data.Data (Data(gmapM))
import safe qualified Data.Set as Set
import safe Data.Foldable (traverse_)

import safe qualified CMM.AST as AST
import safe CMM.AST.Annot (Annot, Annotation(Annot))
import safe CMM.AST.GetName (GetName, getName)
import safe CMM.AST.Variables.State
  ( Collector
  , CollectorState)
import safe qualified CMM.AST.Variables.State as State
import safe CMM.Data.Generics ((<*|*>))
import safe CMM.Inference.TypeKind
  ( TypeKind((:->), Constraint, GenericType, Star)
  )
import safe CMM.Parser.GetPos (GetPos, SourcePos, getPos)

-- | Returns a `CollectorState` containing the local variables of the given node
localVariables ::
     (Data (n SourcePos), Functor n, GetPos a) => n a -> CollectorState
localVariables n = variablesCommon . go $ getPos <$> n
  where
    go :: Data d => d -> Collector d
    go = addTAutoCase $ addCommonCases $ gmapM go

-- | Returns a `CollectorState` containing the global variables of the given `AST.Unit`
globalVariables :: GetPos a => AST.Unit a -> CollectorState
globalVariables n = variablesCommon . go $ getPos <$> n
  where
    go :: Data d => d -> Collector d
    go = addGlobalCases $ addCommonCases $ gmapM go

-- | adds type variables to the `CollectorState` from the given `AST.ParaName`
addParaNameTVars ::
     (GetPos annot, GetName (param annot))
  => Annot (AST.ParaName param) annot
  -> Collector ()
addParaNameTVars (Annot (AST.ParaName _ params) _) =
  traverse_ (`State.addTVarTrivial` GenericType) params

-- | Returns a `CollectorState` containing the local variables of the given `AST.Struct`
structVariables :: GetPos a => AST.Struct a -> CollectorState
structVariables (AST.Struct paraName datums) =
  variablesCommon $ do
    traverse_
      (`State.addSMemTrivial` Star)
      [label | label@(Annot AST.DatumLabel {} _) <- datums]
    addParaNameTVars paraName

-- | Returns a `CollectorState` containing the local variables of the given `AST.Class`
classVariables :: GetPos a => AST.Class a -> CollectorState
classVariables class'@(AST.Class _ paraName _ _) =
  variablesCommon $ do
    addParaNameTVars paraName
    gmapM go $ getPos <$> class'
  where
    go :: Data d => d -> Collector d
    go = goProcedureDecl <*|*> gmapM go
    goProcedureDecl =
      \case
        (procedureDecl :: Annot AST.ProcedureDecl SourcePos) ->
          State.addFVarTrivial procedureDecl Star

-- | Returns a `CollectorState` containing the local variables of the given `AST.Instance`
instanceVariables :: GetPos a => AST.Instance a -> CollectorState
instanceVariables n = variablesCommon . go $ getPos <$> n
  where
    go :: Data d => d -> Collector d
    go = addTAutoCase $ goProcedure <*|*> gmapM go
    goProcedure =
      \case
        (procedure :: Annot AST.Procedure SourcePos) ->
          State.addFVarTrivial procedure Star

-- | Runs the a `Collector` monadic action from the initial state and returns the resulting `CollectorState`
variablesCommon :: Collector a -> CollectorState
variablesCommon = (`execState` State.initCollector)

-- | The type of helper functions that add monotype cases to the polytype function, that monadically collects variables
type CasesAdder m a
   = Data a =>
       (forall d. Data d =>
                    d -> Collector d) -> a -> Collector a

-- | adds the common cases like formals, declarations, registers, datums, etc.
addCommonCases :: CasesAdder m a
addCommonCases go =
  goFormal <*|*> goDecl <*|*> goImport <*|*> goRegisters <*|*> goDatum <*|*>
  goStmt <*|*>
  go
  where
    goFormal =
      \case
        (formal :: Annot AST.Formal SourcePos) ->
          gmapM go formal *> State.addVarTrivial formal Star
    goDecl =
      \case
        decl@(Annot AST.ConstDecl {} (_ :: SourcePos)) -> State.addVarTrivial decl Star
        decl@(Annot (AST.TypedefDecl _ names) (_ :: SourcePos)) ->
          decl <$ traverse_ (`State.addTAlias` GenericType) names
        decl -> gmapM go decl
    goImport =
      \case
        (import' :: Annot AST.Import SourcePos) -> State.addVarTrivial import' Star
    goRegisters =
      \case
        registers@(Annot (AST.Registers _ _ nameStrLits) (_ :: SourcePos)) ->
          registers <$ traverse_ (`State.addVar` Star) (fst <$> nameStrLits)
    goDatum =
      \case
        datum@(Annot AST.DatumLabel {} (_ :: SourcePos)) -> State.addVarTrivial datum Star
        datum -> gmapM go datum
    goStmt =
      \case
        stmt@(Annot AST.LabelStmt {} (_ :: SourcePos)) -> State.addVarTrivial stmt Star
        stmt -> gmapM go stmt

-- | adds the cases specific to the global variable collecting
addGlobalCases :: CasesAdder m a
addGlobalCases go =
  goClass <*|*> goInstance <*|*> goStruct <*|*> goSection <*|*> goProcedure <*|*>
  go
  where
    goClass =
      \case
        class'@(Annot (AST.Class _ (Annot (AST.ParaName _ args) _ ) _ methods) (_ :: SourcePos)) -> do
          traverse_ addMethod methods
          class' <$
            State.addTClass
              class'
              (foldr (:->) Constraint (Star <$ args))
              (Set.fromList $ getName <$> methods)
      where
        addMethod node = do
          State.addFVar node Star
          State.addFIVar node Star
    goInstance =
      \case
        (instance' :: Annot AST.Instance SourcePos) -> return instance'
    goStruct =
      \case
        struct@(Annot (AST.Struct (Annot (AST.ParaName _ args) _) decls) (_ :: SourcePos)) -> do
          State.addTCon struct (foldr (:->) Star (Star <$ args))
          traverse_
            (`State.addSMemTrivial` Star)
            [label | label@(Annot AST.DatumLabel {} _) <- decls]
          return struct
    goSection =
      \case
        (section :: Annot AST.Section SourcePos) -> gmapM goSectionItems section
    goProcedure =
      \case
        (procedure :: Annot AST.Procedure SourcePos) ->
          State.addFVarTrivial procedure Star
    goSectionItems :: Data d => d -> Collector d
    goSectionItems = addSectionCases $ addCommonCases $ gmapM goSectionItems

-- | adds the cases specific to collecting variables inside a section
addSectionCases :: CasesAdder m a
addSectionCases go = goProcedure <*|*> go
  where
    goProcedure =
      \case
        (procedure :: Annot AST.Procedure SourcePos) ->
          State.addFVarTrivial procedure Star <* gmapM goLabels procedure
    goLabels :: Data d => d -> Collector d
    goLabels = goStmt <*|*> goDatum <*|*> gmapM goLabels
    goStmt =
      \case
        stmt@(Annot AST.LabelStmt {} (_ :: SourcePos)) -> State.addVarTrivial stmt Star
        stmt -> gmapM go stmt
    goDatum =
      \case
        datum@(Annot AST.DatumLabel {} (_ :: SourcePos)) -> State.addVarTrivial datum Star
        datum -> gmapM go datum

-- | Adds the case for collecting `AST.TAuto` variables
addTAutoCase :: CasesAdder m a
addTAutoCase go = goTAuto <*|*> go
  where
    goTAuto =
      \case
        tAuto@(Annot (AST.TAuto Nothing) (_ :: SourcePos)) -> return tAuto
        tAuto@(Annot (AST.TAuto Just {}) (_ :: SourcePos)) ->
          tAuto <$ State.addTVar tAuto GenericType
        type' -> gmapM go type'
