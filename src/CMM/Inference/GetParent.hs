{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}

module CMM.Inference.GetParent where

import safe Control.Monad.State (MonadState, modify)
import safe Data.Data (Data)
import safe qualified Data.Set as Set

import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar), TypeVar)
import safe CMM.Inference.Utils (adopt)

-- | Monad for all state monads that can return some kind of parent
class Monad m =>
      GetParent m t
  | m -> t
  where
  getParent :: m t

-- | changes the parent of all occurrences of the type variable in the monadic state
--   represented by the given object to the parent received by getParent
makeAdoption ::
     (GetParent m TypeVar, MonadState s m, Data s, ToTypeVar b)
  => b
  -> (forall d. Data d =>
                  m (d -> d))
makeAdoption withVar = do
  parent <- getParent
  let adopt' ::
           forall d. Data d
        => d
        -> d
      adopt' = adopt parent . Set.singleton $ toTypeVar withVar
  adopt' <$ modify adopt'
