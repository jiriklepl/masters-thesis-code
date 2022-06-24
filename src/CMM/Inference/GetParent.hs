{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}

module CMM.Inference.GetParent where

import safe Control.Monad (Monad)
import safe Control.Monad.State (Monad((>>=), return), MonadState(get, put))
import safe Data.Data (Data)
import safe Data.Function (($), (.))
import safe qualified Data.Set as Set

import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar), TypeVar)
import safe CMM.Inference.Utils (adopt)

class Monad m =>
      GetParent m t
  | m -> t
  where
  getParent :: m t

makeAdoption ::
     (GetParent m TypeVar, MonadState s m, Data s, ToTypeVar b)
  => b
  -> (forall d. Data d =>
                  m (d -> d))
makeAdoption hole = do
  parent <- getParent
  let adopt' ::
           forall d. Data d
        => d
        -> d
      adopt' = adopt parent . Set.singleton $ toTypeVar hole
  get >>= put . adopt'
  return adopt'
