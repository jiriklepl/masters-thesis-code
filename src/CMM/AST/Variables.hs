{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module CMM.AST.Variables where

import safe Control.Lens.Getter ((^.))
import safe Control.Monad.State (MonadIO, StateT, execStateT)
import safe Data.Data (Data(gmapM), Typeable)
import safe Data.Foldable (traverse_)
import safe Data.Generics.Aliases (extM)
import safe Data.Map (Map)
import safe Data.Text (Text)

import safe CMM.AST
  ( Datum(DatumLabel)
  , Decl(ConstDecl, TypedefDecl)
  , Formal
  , Import
  , Procedure
  , Registers(Registers)
  , Section
  , Stmt(LabelStmt)
  , Unit, Struct (Struct), ParaName (ParaName), Class (Class), Type (TAuto)
  )
import safe CMM.AST.Annot (Annot, Annotation(Annot))
import safe CMM.AST.HasName (HasName(getName))
import safe CMM.AST.Variables.State
  ( CollectedVariables
  , MonadCollectVariables
  , addFVarTrivial
  , addTCon
  , addTVar
  , addVar
  , addVarTrivial
  , funcVariables
  , initCollectedVariables
  , typeVariables
  , variables, typeConstants
  )
import safe CMM.Inference.Type (TypeKind(GenericType, Star, (:->), Constraint))
import safe CMM.Parser.HasPos (HasPos(..), SourcePos)

localVariables ::
     (MonadIO m, Data (n SourcePos), Functor n, HasPos a)
  => n a
  -> m ( Map Text (SourcePos, TypeKind)
       , Map Text (SourcePos, TypeKind)
       , Map Text (SourcePos, TypeKind)
       , Map Text (SourcePos, TypeKind))
localVariables n = variablesCommon . go $ getPos <$> n
  where
    go :: (Data d, MonadCollectVariables m) => d -> m d
    go = addTAutoCases $ addCommonCases $ gmapM go

globalVariables ::
     (MonadIO m, HasPos a)
  => Unit a
  -> m ( Map Text (SourcePos, TypeKind)
       , Map Text (SourcePos, TypeKind)
       , Map Text (SourcePos, TypeKind)
       , Map Text (SourcePos, TypeKind))
globalVariables n = variablesCommon . go $ getPos <$> n
  where
    go :: (Data d, MonadCollectVariables m) => d -> m d
    go = addGlobalCases $ addCommonCases $ gmapM go

variablesCommon ::
     MonadIO m
  => StateT CollectedVariables m a
  -> m ( Map Text (SourcePos, TypeKind)
       , Map Text (SourcePos, TypeKind)
       , Map Text (SourcePos, TypeKind)
       , Map Text (SourcePos, TypeKind))
variablesCommon go = do
  result <- execStateT go initCollectedVariables
  return (result ^. variables, result ^. funcVariables, result ^. typeConstants, result ^. typeVariables)

infixr 3 *|*

-- | An alias of flipped `extM`. Its behavior resembles that of the `<|>` method of `Alternative`, including the evaluation order (but mind the infixr fixity).
(*|*) ::
     (Monad m, Typeable a, Typeable b) => (b -> m b) -> (a -> m a) -> a -> m a
(*|*) = flip extM

type CasesAdder m a =
     (Data a, MonadCollectVariables m)
  => (forall d. Data d =>
                  d -> m d)
  -> a
  -> m a

addCommonCases :: CasesAdder m a
addCommonCases go =
  goFormal *|* goDecl *|* goImport *|* goRegisters *|* goDatum *|* goStmt *|* go
  where
    goFormal =
      \case
        (formal :: Annot Formal SourcePos) -> gmapM go formal *> addVarTrivial formal Star
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
addGlobalCases go = goClass *|* goStruct *|* goProcedure *|* goSection *|* go
  where
    goClass =
      \case
        class'@(Annot (Class _ (Annot (ParaName _ args) _) _) (_ :: SourcePos)) -> do
          addTCon class' (getName class') (foldr (:->) Constraint (Star <$ args))
          gmapM go class'
    goStruct =
      \case
        struct@(Annot (Struct (Annot (ParaName _ args) _) _) (_ :: SourcePos)) -> do
          struct <$ addTCon struct (getName struct) (foldr (:->) Star (Star <$ args))
    goProcedure =
      \case
        (procedure :: Annot Procedure SourcePos) ->
          addFVarTrivial procedure Star
    goSection =
      \case
        (section :: Annot Section SourcePos) -> gmapM goSectionItems section
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
        tAuto@(Annot (TAuto (Just n)) (_ :: SourcePos)) -> tAuto <$ addTVar tAuto (getName n) GenericType
        type' -> gmapM go type'
