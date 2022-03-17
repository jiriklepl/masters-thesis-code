{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module CMM.Monomorphize where

import safe Control.Applicative (Applicative(liftA2))
import safe Control.Lens.Getter ((^.), uses)
import qualified Data.Bimap as Bimap
import safe qualified Data.PartialOrd as PartialOrd

import safe CMM.AST (Name)
import safe CMM.AST.Annot (Annot, Annotation(Annot), withAnnot)
import safe CMM.AST.Maps (ASTmap(..), ASTmapGen, Constraint, Space)
import safe CMM.Data.Bounds (Bounds(Bounds))
import safe CMM.Inference.Preprocess.State (HasTypeHandle)
import safe CMM.Inference.State
  ( MonadInferencer
  , handlize
  , readConstingBounds
  , readKindingBounds
  , unifs
  )
import safe CMM.Inference.Subst (Apply(apply))
import safe CMM.Inference.Type (IsTyped(freeTypeVars), Type, TypeVar)
import safe CMM.Inference.TypeHandle (consting, kinding, typing)
import safe CMM.Monomorphize.PolyKind (PolyKind(..))
import safe CMM.Parser.HasPos (HasPos)
import safe CMM.Utils (backQuote)

class Monomorphize n a where
  monomorphize ::
       (HasPos a, HasTypeHandle a, MonadInferencer m) => n a -> m (n a)

data MonomorphizeHint =
  MonomorphizeHint

type instance Constraint MonomorphizeHint a b =
     (HasPos a, HasTypeHandle a)

type instance Space MonomorphizeHint = Monomorphize'

class Monomorphize' a b n where
  monomorphize' ::
       (HasPos a, HasTypeHandle a, MonadInferencer m) => n a -> m (n b)

instance {-# OVERLAPPABLE #-} Monomorphize (Annot n) a =>
                              Monomorphize' a a (Annot n) where
  monomorphize' = monomorphize

instance Monomorphize' a a Name where
  monomorphize' = return

instance ASTmapGen MonomorphizeHint a a

instance {-# OVERLAPPABLE #-} (ASTmap MonomorphizeHint n a a) =>
                              Monomorphize (Annot n) a where
  monomorphize (Annot n a) =
    withAnnot a <$> astMapM MonomorphizeHint monomorphize' n

typingPolyKind :: Type -> PolyKind
typingPolyKind t =
  if null $ freeTypeVars t
    then Mono
    else Poly

constnessPolyKind :: MonadInferencer m => TypeVar -> m PolyKind
constnessPolyKind tVar = go <$> readConstingBounds tVar
  where
    go (low `Bounds` high) =
      case low `compare` high of
        LT -> Poly
        EQ -> Mono
        GT -> Absurd

kindingPolyKind :: MonadInferencer m => TypeVar -> m PolyKind
kindingPolyKind tVar = go <$> readKindingBounds tVar
  where
    go (low `Bounds` high) =
      if low PartialOrd.<= high
        then if high PartialOrd.<= low
               then Mono
               else Poly
        else Absurd

typePolyKind :: MonadInferencer m => TypeVar -> m PolyKind
typePolyKind tVar =
  uses handlize (flip Bimap.lookup) <*> uses unifs (`apply` tVar) >>= \case
    Just handle ->
      mappend (typingPolyKind $ handle ^. typing) <$>
      liftA2
        mappend
        (kindingPolyKind $ handle ^. kinding)
        (kindingPolyKind $ handle ^. consting)
    Nothing ->
      error $
      "(internal logic error) Type variable " <>
      backQuote (show tVar) <> " not registered by the inferencer."
