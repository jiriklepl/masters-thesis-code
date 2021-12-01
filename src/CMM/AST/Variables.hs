{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module CMM.AST.Variables where

import safe Control.Lens.Getter
import safe Control.Monad.State
import safe Data.Data
import safe Data.Foldable
import safe Data.Generics.Aliases
import safe Data.Map (Map)
import safe Data.Text (Text)

import safe CMM.AST
import safe CMM.Inference.Type
import safe CMM.AST.Annot
import safe CMM.AST.HasName
import safe CMM.AST.Variables.State
import safe CMM.Parser.HasPos

localVariables :: (MonadIO m, HasPos a) => Procedure a -> m (Map Text TypeKind, Map Text TypeKind)
localVariables n = variablesCommon . go $ getPos <$> n
  where
    go :: (Data d, MonadCollectVariables m) => d -> m d
    go = addCommonCases $ gmapM go

globalVariables :: (MonadIO m, HasPos a) => Unit a -> m (Map Text TypeKind, Map Text TypeKind)
globalVariables n = variablesCommon . go $ getPos <$> n
  where
    go :: (Data d, MonadCollectVariables m) => d -> m d
    go = addGlobalCases $ addCommonCases $ gmapM go

variablesCommon ::
     MonadIO m => StateT CollectedVariables m a -> m (Map Text TypeKind, Map Text TypeKind)
variablesCommon go = do
  result <- execStateT go initCollectedVariables
  return (result ^. variables, result ^. typeVariables)

infixr 3 $|

-- | An alias of flipped `extM`. Its behavior resembles that of the `<|>` method of `Alternative`, including the evaluation order (but mind the infixr fixity).
($|) ::
     (Monad m, Typeable a, Typeable b) => (b -> m b) -> (a -> m a) -> a -> m a
($|) = flip extM

addCommonCases ::
     (Data a, MonadCollectVariables m)
  => (forall d. Data d =>
                  d -> m d)
  -> a
  -> m a
addCommonCases go =
  goFormal $| goDecl $| goImport $| goRegisters $| goDatum $| goStmt $| go
  where
    goFormal =
      \case
        (formal :: Annot Formal SourcePos) -> addVarTrivial formal Star
    goDecl =
      \case
        decl@(Annot ConstDecl {} (_ :: SourcePos)) -> addVarTrivial decl Star
        decl@(Annot (TypedefDecl _ names) (_ :: SourcePos)) ->
          decl <$ traverse_ (flip (addTVar decl) Generic) (getName <$> names)
        decl -> gmapM go decl
    goImport =
      \case
        (import' :: Annot Import SourcePos) -> addVarTrivial import' Star
    goRegisters =
      \case
        registers@(Annot (Registers _ _ nameStrLits) (_ :: SourcePos)) ->
          registers <$
          traverse_ (flip (addVar registers) Star) (getName . fst <$> nameStrLits)
    goDatum =
      \case
        datum@(Annot DatumLabel {} (_ :: SourcePos)) -> addVarTrivial datum Star
        datum -> gmapM go datum
    goStmt =
      \case
        stmt@(Annot LabelStmt {} (_ :: SourcePos)) -> addVarTrivial stmt Star
        stmt -> gmapM go stmt

addGlobalCases ::
     (Data a, MonadCollectVariables m)
  => (forall d. Data d =>
                  d -> m d)
  -> a
  -> m a
addGlobalCases go = goProcedure $| goSection $| go
  where
    goProcedure =
      \case
        (procedure :: Annot Procedure SourcePos) -> addVarTrivial procedure Star
    goSection =
      \case
        (section :: Annot Section SourcePos) -> gmapM goSectionItems section
    goSectionItems :: (Data d, MonadCollectVariables m) => d -> m d
    goSectionItems = addSectionCases $ addCommonCases $ gmapM goSectionItems

addSectionCases ::
     (Data a, MonadCollectVariables m)
  => (forall d. Data d =>
                  d -> m d)
  -> a
  -> m a
addSectionCases go = goProcedure $| go
  where
    goProcedure =
      \case
        (procedure :: Annot Procedure SourcePos) ->
          addVarTrivial procedure Star <* gmapM goLabels procedure
    goLabels :: (Data d, MonadCollectVariables m) => d -> m d
    goLabels = addLabelCases $ gmapM goLabels

addLabelCases ::
     (Data a, MonadCollectVariables m)
  => (forall d. Data d =>
                  d -> m d)
  -> a
  -> m a
addLabelCases go = goStmt $| goDatum $| go
  where
    goStmt =
      \case
        stmt@(Annot LabelStmt {} (_ :: SourcePos)) -> addVarTrivial stmt Star
        stmt -> gmapM go stmt
    goDatum =
      \case
        datum@(Annot DatumLabel {} (_ :: SourcePos)) -> addVarTrivial datum Star
        datum -> gmapM go datum
