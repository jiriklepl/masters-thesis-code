{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module CMM.AST.Variables where

import Control.Monad.State
import Control.Lens.Setter
import Control.Lens.Getter
import Data.Data
import Data.Generics.Aliases
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Foldable
import Control.Lens.TH (makeLenses)
import Prettyprinter

import CMM.AST
import CMM.AST.Annot
import CMM.AST.HasName
import CMM.Parser.HasPos
import CMM.Pretty()
import CMM.Warnings

data CollectedVariables = CollectedVariables
  { _variables :: Set Text
  , _typeVariables :: Set Text
  , _errors :: Int
  , _warnings :: Int
  }

initCollectedVariables :: CollectedVariables
initCollectedVariables = CollectedVariables
  { _variables = mempty
  , _typeVariables = mempty
  , _errors = 0
  , _warnings = 0
  }

type MonadCollectVariables m = (MonadState CollectedVariables m, MonadIO m)

makeLenses ''CollectedVariables

registerError :: (HasPos n, Pretty n, MonadCollectVariables m) => n -> Text -> m ()
registerError node message = do
  errors += 1
  makeMessage mkError node message

registerWarning :: (HasPos n, Pretty n, MonadCollectVariables m) => n -> Text -> m ()
registerWarning node message = do
  warnings += 1
  makeMessage mkWarning node message

addVar :: (HasPos n, Pretty n, MonadCollectVariables m) => n -> Text -> m ()
addVar node var = do
  uses variables (var `Set.member`) >>= \case
    True -> registerError node "Duplicate variable"
    False -> variables %= Set.insert var

addVarTrivial :: (HasPos n, Pretty n, HasName n, MonadCollectVariables m) =>
  n -> m n
addVarTrivial n = n <$ addVar n (getName n)

addTVar :: (HasPos n, Pretty n, MonadCollectVariables m) => n -> Text -> m ()
addTVar node tVar = do
  uses variables (tVar `Set.member`) >>= \case
    True -> registerError node "Duplicate type variable"
    False -> variables %= Set.insert tVar

addTVarTrivial :: (HasPos n, Pretty n, HasName n, MonadCollectVariables m) =>
  n -> m n
addTVarTrivial n = n <$ addTVar n (getName n)

variablesCommon :: MonadIO m => StateT CollectedVariables m a -> m (Set Text, Set Text)
variablesCommon go = do
  result <- execStateT go initCollectedVariables
  return (result ^. variables, result ^. typeVariables)

localVariables :: (MonadIO m, HasPos a) => Procedure a -> m (Set Text, Set Text)
localVariables n = variablesCommon . go $ getPos <$> n
  where go :: (Data d, MonadCollectVariables m) => d -> m d
        go = addCommonCases $ gmapM go

addCommonCases :: (Data a, MonadCollectVariables m) => (forall d . Data d => d -> m d) -> a -> m a
addCommonCases go = go
  `extM` \case (formal :: Annot Formal SourcePos) -> addVarTrivial formal
  `extM` \case decl@(Annot ConstDecl{} (_ :: SourcePos)) -> addVarTrivial decl
               decl@(Annot (TypedefDecl _ names) (_ :: SourcePos)) -> decl <$ traverse_ (addTVar decl) (getName <$> names)
               decl -> gmapM go decl
  `extM` \case (import' :: Annot Import SourcePos) -> addVarTrivial import'
  `extM` \case registers@(Annot (Registers _ _ nameStrLits) (_ :: SourcePos)) -> registers <$ traverse_ (addVar registers) (getName . fst <$> nameStrLits)
  `extM` \case datum@(Annot DatumLabel{} (_ :: SourcePos)) -> addVarTrivial datum
               datum -> gmapM go datum
  `extM` \case stmt@(Annot LabelStmt{} (_ :: SourcePos)) -> addVarTrivial stmt
               stmt -> gmapM go stmt

globalVariables :: (MonadIO m, HasPos a) => Unit a -> m (Set Text, Set Text)
globalVariables n = variablesCommon . go $ getPos <$> n
  where go :: (Data d, MonadCollectVariables m) => d -> m d
        go = addCommonCases $ gmapM go
          `extM` \case (procedure :: Annot Procedure SourcePos) -> addVarTrivial procedure
          `extM` \case (section :: Annot Section SourcePos) -> goSection section

        goSection :: (Data d, MonadCollectVariables m) => d -> m d
        goSection = addCommonCases $ gmapM goSection
          `extM` \case (procedure :: Annot Procedure SourcePos) -> addVarTrivial procedure <* gmapM goLabels procedure

        goLabels :: (Data d, MonadCollectVariables m) => d -> m d
        goLabels = gmapM goLabels
          `extM` \case stmt@(Annot LabelStmt{} (_ :: SourcePos)) -> addVarTrivial stmt
                       stmt -> gmapM go stmt
          `extM` \case datum@(Annot DatumLabel{} (_ :: SourcePos)) -> addVarTrivial datum
                       datum -> gmapM go datum
