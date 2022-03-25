{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module CMM.Monomorphize.Schematized where

import safe CMM.AST ( Procedure, Struct, AST )
import safe Data.Data ( Data )

data Schematized a
  = FuncScheme (Procedure a)
  | StructScheme (Struct a)
  deriving (Show, Functor, Foldable, Traversable, Data, AST)

deriving instance Eq (Schematized ())
