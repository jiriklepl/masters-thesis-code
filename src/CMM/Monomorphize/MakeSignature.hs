{-# LANGUAGE Safe #-}

module CMM.Monomorphize.MakeSignature where

import Prelude

import CMM.Inference.Preprocess.TypeHole
import CMM.Monomorphize.Schematized
import CMM.AST.Annot
import CMM.AST hiding (Type)
import CMM.Inference.Subst
import CMM.Inference.Type
import CMM.Inference.State
import CMM.Inference.TypeVar
import CMM.Inference
import qualified Data.Map as Map
import CMM.Inference.State.Impl
import Control.Lens
import qualified CMM.Data.Bimap as Bimap
import CMM.Inference.TypeCompl
import CMM.Inference.TypeHandle
import Prettyprinter
import CMM.Pretty

makeSignature subst a = do
    tVar <- reconstructOld (toTypeVar . holeHandle $ getTypeHole a) >>=
      simplify . apply subst
    uses typize (Bimap.lookup tVar) >>= \case
      Just (FunctionType args ret) -> do
        uses typize (Bimap.lookup ret) >>= \case
          Just (TupleType rets) -> do
            args' <- traverse goVar args
            rets' <- traverse goVar rets
            return . show $ tupled args' <+> arrowNice <+> tupled (reverse rets')
          _ -> undefined
      _ -> undefined
    where
      goVar tVar = do
        handle <- getHandle tVar
        return $ pretty (view typing handle)
