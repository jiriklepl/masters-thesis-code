{-# LANGUAGE Safe #-}

module CMM.Inference.DataKind where

import safe Data.Bool (Bool(False, True), otherwise)
import safe Data.Data (Data)
import safe Data.Eq (Eq)
import safe Data.Foldable (Foldable(null))
import safe Data.Function (($))
import safe Data.Int (Int)
import safe Data.Monoid (Monoid(mempty), (<>))
import safe Data.Ord (Ord(compare, min), Ordering(EQ, GT, LT))
import safe Data.PartialOrd (PartialOrd)
import safe qualified Data.PartialOrd as PartialOrd
import safe Data.Semigroup (Semigroup)
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Prelude (Bounded(maxBound, minBound))
import safe Text.Show (Show)

import safe CMM.Data.Dioid (Dioid((<.>), mfull))
import safe CMM.Data.Lattice (Lattice((/\), (\/)))
import safe CMM.Data.Nullable (Fallbackable((??)), Nullable(nullVal))
import safe CMM.Data.Ordered (Ordered(Ordered))

-- | DataKind specifies the semantics and register allocability of the types that map onto it via kinding
data DataKind
  = GenericData -- | The most generic data kind
  | Unstorable -- | The empty data kind
  | DataKind (Set Int) -- | The regular case for data kinds
  | FunctionKind Int
  | TupleKind Int
  deriving (Show, Eq, Data)

instance PartialOrd DataKind where
  _ <= GenericData = True
  GenericData <= _ = False
  Unstorable <= _ = True
  _ <= Unstorable = False
  DataKind rs <= DataKind rs' = rs PartialOrd.<= rs'
  FunctionKind int <= FunctionKind int' = int PartialOrd.<= int'
  TupleKind int <= TupleKind int' = int PartialOrd.<= int'
  _ <= _ = False

instance Fallbackable DataKind where
  Unstorable ?? a = a
  a ?? _ = a

instance Nullable DataKind where
  nullVal = Unstorable

instance Bounded DataKind where
  minBound = Unstorable
  maxBound = GenericData

-- | Transforms the given `Set` of `Int`s denoting physical registers into a corresponding `DataKind`
makeDataKind :: Set Int -> DataKind
makeDataKind set
  | null set = Unstorable
  | otherwise = DataKind set

instance Lattice DataKind where
  GenericData /\ a = a
  Unstorable /\ _ = Unstorable
  DataKind rs /\ DataKind rs' = makeDataKind $ rs `Set.intersection` rs'
  FunctionKind int /\ FunctionKind int' = FunctionKind $ min int int'
  TupleKind int /\ TupleKind int' = TupleKind $ min int int'
  FunctionKind {} /\ _ = Unstorable
  TupleKind {} /\ _ = Unstorable
  a /\ b = b /\ a
  GenericData \/ _ = GenericData
  Unstorable \/ a = a
  DataKind rs \/ DataKind rs' = makeDataKind $ rs <> rs'
  FunctionKind int \/ FunctionKind int' = FunctionKind $ min int int'
  TupleKind int \/ TupleKind int' = TupleKind $ min int int'
  FunctionKind {} \/ _ = GenericData
  TupleKind {} \/ _ = GenericData
  a \/ b = b \/ a

instance Semigroup DataKind where
  (<>) = (\/)

instance Monoid DataKind where
  mempty = Unstorable

instance Dioid DataKind where
  (<.>) = (/\)
  mfull = GenericData

instance Ord (Ordered DataKind) where
  Ordered GenericData `compare` Ordered GenericData = EQ
  Ordered Unstorable `compare` Ordered Unstorable = EQ
  Ordered (DataKind set) `compare` Ordered (DataKind set') = set `compare` set'
  Ordered (FunctionKind int) `compare` Ordered (FunctionKind int') =
    int `compare` int'
  Ordered (TupleKind int) `compare` Ordered (TupleKind int') =
    int `compare` int'
  Ordered GenericData `compare` _ = LT
  _ `compare` Ordered GenericData = GT
  Ordered Unstorable `compare` _ = LT
  _ `compare` (Ordered Unstorable) = GT
  Ordered DataKind {} `compare` _ = LT
  _ `compare` (Ordered DataKind {}) = GT
  Ordered FunctionKind {} `compare` _ = LT
  _ `compare` (Ordered FunctionKind {}) = GT

instance Bounded (Ordered DataKind) where
  minBound = Ordered minBound
  maxBound = Ordered maxBound
