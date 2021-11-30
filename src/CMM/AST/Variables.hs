{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module CMM.AST.Variables where

import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.TH (makeLenses)
import Control.Monad.State
import Data.Data
import Data.Foldable
import Data.Generics.Aliases
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Prettyprinter

import CMM.AST
import CMM.AST.Annot
import CMM.AST.HasName
import CMM.Parser.HasPos
import CMM.Pretty ()
import CMM.Warnings

data CollectedVariables =
  CollectedVariables
    { _variables :: Set Text
    , _typeVariables :: Set Text
    , _errors :: Int
    , _warnings :: Int
    }

initCollectedVariables :: CollectedVariables
initCollectedVariables =
  CollectedVariables
    {_variables = mempty, _typeVariables = mempty, _errors = 0, _warnings = 0}

type MonadCollectVariables m = (MonadState CollectedVariables m, MonadIO m)

makeLenses ''CollectedVariables

registerError ::
     (HasPos n, Pretty n, MonadCollectVariables m) => n -> Text -> m ()
registerError node message = do
  errors += 1
  makeMessage mkError node message

registerWarning ::
     (HasPos n, Pretty n, MonadCollectVariables m) => n -> Text -> m ()
registerWarning node message = do
  warnings += 1
  makeMessage mkWarning node message

addVar :: (HasPos n, Pretty n, MonadCollectVariables m) => n -> Text -> m ()
addVar node var = do
  uses variables (var `Set.member`) >>= \case
    True -> registerError node "Duplicate variable"
    False -> variables %= Set.insert var

addVarTrivial ::
     (HasPos n, Pretty n, HasName n, MonadCollectVariables m) => n -> m n
addVarTrivial n = n <$ addVar n (getName n)

addTVar :: (HasPos n, Pretty n, MonadCollectVariables m) => n -> Text -> m ()
addTVar node tVar = do
  uses variables (tVar `Set.member`) >>= \case
    True -> registerError node "Duplicate type variable"
    False -> variables %= Set.insert tVar

addTVarTrivial ::
     (HasPos n, Pretty n, HasName n, MonadCollectVariables m) => n -> m n
addTVarTrivial n = n <$ addTVar n (getName n)

variablesCommon ::
     MonadIO m => StateT CollectedVariables m a -> m (Set Text, Set Text)
variablesCommon go = do
  result <- execStateT go initCollectedVariables
  return (result ^. variables, result ^. typeVariables)

localVariables :: (MonadIO m, HasPos a) => Procedure a -> m (Set Text, Set Text)
localVariables n = variablesCommon . go $ getPos <$> n
  where
    go :: (Data d, MonadCollectVariables m) => d -> m d
    go = addCommonCases $ gmapM go

globalVariables :: (MonadIO m, HasPos a) => Unit a -> m (Set Text, Set Text)
globalVariables n = variablesCommon . go $ getPos <$> n
  where
    go :: (Data d, MonadCollectVariables m) => d -> m d
    go = addGlobalCases $ addCommonCases $ gmapM go

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
        (formal :: Annot Formal SourcePos) -> addVarTrivial formal
    goDecl =
      \case
        decl@(Annot ConstDecl {} (_ :: SourcePos)) -> addVarTrivial decl
        decl@(Annot (TypedefDecl _ names) (_ :: SourcePos)) ->
          decl <$ traverse_ (addTVar decl) (getName <$> names)
        decl -> gmapM go decl
    goImport =
      \case
        (import' :: Annot Import SourcePos) -> addVarTrivial import'
    goRegisters =
      \case
        registers@(Annot (Registers _ _ nameStrLits) (_ :: SourcePos)) ->
          registers <$
          traverse_ (addVar registers) (getName . fst <$> nameStrLits)
    goDatum =
      \case
        datum@(Annot DatumLabel {} (_ :: SourcePos)) -> addVarTrivial datum
        datum -> gmapM go datum
    goStmt =
      \case
        stmt@(Annot LabelStmt {} (_ :: SourcePos)) -> addVarTrivial stmt
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
        (procedure :: Annot Procedure SourcePos) -> addVarTrivial procedure
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
          addVarTrivial procedure <* gmapM goLabels procedure
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
        stmt@(Annot LabelStmt {} (_ :: SourcePos)) -> addVarTrivial stmt
        stmt -> gmapM go stmt
    goDatum =
      \case
        datum@(Annot DatumLabel {} (_ :: SourcePos)) -> addVarTrivial datum
        datum -> gmapM go datum
