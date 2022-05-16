{-# LANGUAGE Safe #-}

module CMM.Inference.TypeAnnot where

import safe Data.Bool (otherwise)
import safe Data.Data (Data)
import safe Data.Eq (Eq((==)))
import safe Data.Function (($))
import safe Data.Monoid (Monoid(mempty))
import safe Data.Ord (Ord)
import safe Data.Semigroup (Semigroup((<>)))
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)
import safe Text.Show (Show)

import safe CMM.Data.Nullable (Fallbackable((??)), Nullable(nullVal))
import safe CMM.Inference.TypeVar (TypeVar)
import safe CMM.Parser.HasPos (SourcePos)

data TypeAnnot
  = NoTypeAnnot
  | TypeInst TypeVar
  | TypeAST SourcePos
  | TypeNamed Text
  | TypeNamedAST Text SourcePos -- TODO: delete this one
  | TypeBuiltIn Text
  | MultiAnnot (Set TypeAnnot)
  deriving (Eq, Ord, Show, Data)

instance Fallbackable TypeAnnot where
  NoTypeAnnot ?? kind = kind
  kind ?? _ = kind

instance Nullable TypeAnnot where
  nullVal = NoTypeAnnot

instance Semigroup TypeAnnot where
  MultiAnnot annots <> MultiAnnot annots' = MultiAnnot $ annots <> annots'
  MultiAnnot annots <> annot = MultiAnnot $ Set.insert annot annots
  annot <> MultiAnnot annots = MultiAnnot $ Set.insert annot annots
  annot <> annot'
    | annot == annot' = annot
    | otherwise = MultiAnnot (Set.fromList [annot, annot'])

instance Monoid TypeAnnot where
  mempty = NoTypeAnnot
