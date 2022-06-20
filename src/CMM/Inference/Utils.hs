{-# LANGUAGE Safe #-}

module CMM.Inference.Utils where
import safe Data.Data ( Data(gmapT) )
import safe qualified Data.Set as Set
import safe Data.Set ( Set )
import safe Data.Bool ( otherwise )

import safe CMM.Inference.TypeVar ( TypeVar(tVarParent) )
import safe CMM.Data.Generics ( (*|*) )

adopt :: Data d => TypeVar -> Set TypeVar -> d -> d
adopt parent children = go
  where
    go :: Data d => d -> d
    go = tVarCase *|* gmapT go
    tVarCase tVar
      | tVar `Set.member` children = tVar{tVarParent=parent}
      | otherwise = tVar
