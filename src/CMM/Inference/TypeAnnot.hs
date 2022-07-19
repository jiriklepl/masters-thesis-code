{-# LANGUAGE Safe #-}

module CMM.Inference.TypeAnnot where

import safe Data.Data (Data)
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)

import safe Prettyprinter (Pretty(pretty), dquotes, parens)

import safe CMM.Data.Nullable (Fallbackable((??)), Nullable(nullVal))
import safe CMM.Inference.TypeVar (TypeVar)
import safe CMM.Parser.GetPos (SourcePos)
import CMM.Pretty (commaSep, genSymbol)

-- | annotations used in type properties
data TypeAnnot
  = NoTypeAnnot -- ^ no annotation
  | TypeInst TypeVar -- ^ represents an monotype instance of the given polytype type variable
  | TypeAST SourcePos -- ^ relates to a node at the given location
  | TypeNamedAST Text SourcePos -- ^ relates to a node at the given location and has a name
  | MultiAnnot (Set TypeAnnot) -- ^ more annotations collected together
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

instance Pretty TypeAnnot where
  pretty =
    parens . \case
      NoTypeAnnot -> mempty
      TypeInst poly -> genSymbol <> pretty poly
      TypeAST {} -> mempty
      TypeNamedAST name _ -> dquotes $ pretty name
      MultiAnnot set -> commaSep . fmap pretty $ Set.toList set
