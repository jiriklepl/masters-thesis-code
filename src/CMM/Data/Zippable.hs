{-# LANGUAGE Safe #-}

module CMM.Data.Zippable where

import safe qualified Data.List as List
import safe Data.Maybe ( Maybe (Just, Nothing) )
import safe Control.Applicative ( Applicative(pure) )
import safe Data.Function ( ($), (.) )
import safe Data.Either ( Either(Left,Right) )
import safe Data.Monoid ( Monoid, (<>) )

class Zippable z where
  zipWith :: (a -> b -> c) -> z a -> z b -> Maybe (z c)

instance Zippable [] where
  zipWith f a b = pure $ List.zipWith f a b

instance Zippable Maybe where
  zipWith f (Just a) (Just b) = Just . Just $ f a b
  zipWith _ Nothing Nothing = Just Nothing
  zipWith _ _ _ = Nothing

instance Monoid e => Zippable (Either e) where
  zipWith f (Right a) (Right b) = Just . Right $ f a b
  zipWith _ (Left a) (Left b) = Just . Left $ a <> b
  zipWith _ _ _ = Nothing

