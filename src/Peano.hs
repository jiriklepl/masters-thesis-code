{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Peano where

data Zero = Zero
newtype Succ a = Succ a

type P0 = Zero
p0 :: Zero
p0 = Zero

type P1 = Succ P0
p1 :: P1
p1 = Succ p0


type P2 = Succ P1
p2 :: P2
p2 = Succ p1

type P3 = Succ P2
p3 :: P3
p3 = Succ p2

type P4 = Succ P3
p4 :: P4
p4 = Succ p3

type P5 = Succ P4
p5 :: P5
p5 = Succ p4

type P6 = Succ P5
p6 :: P6
p6 = Succ p5

type P7 = Succ P6
p7 :: P7
p7 = Succ p6

type P8 = Succ P7
p8 :: P8
p8 = Succ p7

type P9 = Succ P8
p9 :: P9
p9 = Succ p8

class Nat n
instance Nat Zero
instance Nat n => Nat (Succ n)
