{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module CMM.Monomorphize where
import CMM.Inference.State (MonadInferencer, unifs, handlize)
import CMM.Parser.HasPos
import CMM.Inference.Preprocess.State
import CMM.AST.Maps
import CMM.AST.Annot
import CMM.AST (Name)
import CMM.Inference.Type (TypeVar, Type, IsTyped (freeTypeVars))
import CMM.Inference.Subst (Apply(apply))
import Control.Lens.Getter (uses)
import qualified Data.Bimap as Bimap
import CMM.Utils (backQuote)


class Monomorphize n a where
  monomorphize :: (HasPos a, HasTypeHandle a, MonadInferencer m) => n a -> m (n a)

data MonomorphizeHint = MonomorphizeHint

type instance Constraint MonomorphizeHint a b = (HasPos a, HasTypeHandle a)
type instance Space MonomorphizeHint = Monomorphize'

class Monomorphize' a b n where
  monomorphize' :: (HasPos a, HasTypeHandle a, MonadInferencer m) => n a -> m (n b)

instance {-# OVERLAPPABLE #-} Monomorphize (Annot n) a => Monomorphize' a a (Annot n) where
  monomorphize' = monomorphize

instance Monomorphize' a a Name where
  monomorphize' = return

instance ASTmapGen MonomorphizeHint a a

instance {-# OVERLAPPABLE #-} (ASTmap MonomorphizeHint n a a) =>
                              Monomorphize (Annot n) a where
  monomorphize (Annot n annot) = withAnnot annot <$> astMapM MonomorphizeHint monomorphize' n

isMonoTyping :: Type -> Bool
isMonoTyping t = null $ freeTypeVars t

isMonoConstness :: MonadInferencer m => TypeVar -> m Bool
isMonoConstness = undefined

isMonoKinding :: MonadInferencer m => TypeVar -> m Bool
isMonoKinding = undefined


isMonoType :: MonadInferencer m => TypeVar -> m Bool
isMonoType tVar = do
  uses handlize (flip Bimap.lookup) <*> uses unifs (`apply` tVar) >>= \case
    Just handle -> do
      undefined
    Nothing -> error $ "(internal logic error) Type variable " <> backQuote (show tVar) <> " not registered by the inferencer."
  return undefined
