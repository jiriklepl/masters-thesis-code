{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module CMM.Monomorphize.Error where

import safe Data.Data ( Data )
import safe Data.Eq ( Eq )
import safe Data.Int ( Int )
import safe Text.Show ( Show )

import safe Prettyprinter
    ( (<>), Pretty(pretty), (<+>), list, parens )

import safe CMM.Err.IsError ( IsError )
import safe CMM.Monomorphize.Polytypeness ( PolyWhat, Absurdity )
import safe CMM.Inference.Preprocess.TypeHole ( TypeHole )
import safe CMM.Inference.TypeVar ( TypeVar )
import safe CMM.Inference.Type ( Type )
import CMM.AST.Wrap (ASTWrapper, MakeWrapped (makeWrapped))
import safe CMM.AST.Annot
    ( Annotation(Annot, takeAnnot), Annot, mapAnnot )
import CMM.Inference.Unify.Error (UnificationError)
import safe CMM.Parser.HasPos ( HasPos )
import CMM.Err.Error (Error)
import qualified CMM.Err.Error as Error
import safe Data.Functor ( (<$>), void )
import safe Data.Function ( ($), (.) )
import safe CMM.Parser.ASTError ( makeASTError )
import safe CMM.Monomorphize.Schematized ( Schematized (FuncScheme, StructScheme) )

data MonomorphizeError
  = ReachedMaxWaves Int
  | AbsurdType Absurdity (Annot ASTWrapper ())
  | IllegalPolyType PolyWhat (Annot ASTWrapper ())
  | InstantiatesToNothing (Annot ASTWrapper ())
  | IllegalHole TypeHole (Annot ASTWrapper ())
  | IllegalScheme (Schematized ()) (Annot ASTWrapper ())
  | NoInstance TypeVar TypeVar (Annot ASTWrapper ())
  | IsNotScheme (Annot ASTWrapper ())
  | CannotInstantiate Type Type [UnificationError] (Annot ASTWrapper ())
  | Ambiguity [TypeHole] (Annot ASTWrapper ())
  deriving (Show, Eq, IsError, Data)

instance Pretty MonomorphizeError where
  pretty = \case
    ReachedMaxWaves int -> "Reached the maximum number of monomorphization waves" <+> parens (pretty int)
    AbsurdType absurdity node -> pretty node <+> "has absurd type:" <+> pretty absurdity
    IllegalPolyType polyWhat node -> pretty node <+> "is poly-typed in the context that requires monotypes:" <+> pretty polyWhat
    InstantiatesToNothing node -> pretty node <+> "instantiates into nothing"
    IllegalHole hole node -> pretty node <+> "has an illegal type hole" <+> parens (pretty hole) <> report
    IllegalScheme schematized node -> pretty node <+> case schematized of
      FuncScheme func -> "Is required to be a structure, but has been given a function:" <+> pretty func
      StructScheme struct -> "Is required to be a function, but has been given a structure:" <+> pretty struct
    NoInstance scheme inst node -> pretty node <+> "with a scheme" <+> pretty scheme <+> "failed to instantiate into" <+> pretty inst
    IsNotScheme node -> "There is no scheme registered for" <+> pretty node <> report
    CannotInstantiate scheme inst unifErrors node ->
      pretty node <+> "cannot be instantiated from the type" <+> pretty scheme <+>
      "to the type" <+> pretty inst <+> "due to" <> list (pretty <$> unifErrors) <> report
    Ambiguity holes node -> case holes of
      [_] -> pretty node <+> " was not instantiated due to broken contract in monomorphization"
      [] -> pretty node <+> " cannot be instantiated, there are no suitable schemes"
      _ -> pretty node <+> " cannot be instantiated due to ambiguity between the following types:" <+> list (pretty <$> holes)
    where report = ", report bug in inference preprocessing"

makeError :: HasPos n => n -> MonomorphizeError -> Error
makeError n = Error.makeError . makeASTError n

mapWrapped :: MakeWrapped n => Annot n a -> Annotation ASTWrapper ()
mapWrapped = void . mapAnnot makeWrapped

instantiatesToNothing :: (HasPos a, MakeWrapped n) => Annotation n a -> Error
instantiatesToNothing annotated@Annot{takeAnnot} =
  makeError takeAnnot . InstantiatesToNothing $ mapWrapped annotated

illegalNothing :: (HasPos a, MakeWrapped n) => Annot n a -> Error
illegalNothing annotated@Annot{takeAnnot} =
  makeError takeAnnot . InstantiatesToNothing $ mapWrapped annotated

absurdType :: (HasPos a, MakeWrapped n) =>
  Absurdity -> Annotation n a -> Error
absurdType absurdity annotated@Annot{takeAnnot} =
  makeError takeAnnot . AbsurdType absurdity $ mapWrapped annotated

illegalPolyType :: (HasPos a, MakeWrapped n) =>
  PolyWhat -> Annotation n a -> Error
illegalPolyType absurdity annotated@Annot{takeAnnot} =
  makeError takeAnnot . IllegalPolyType absurdity $ mapWrapped annotated

illegalHole :: (HasPos a, MakeWrapped n) => TypeHole -> Annotation n a -> Error
illegalHole hole annotated@Annot{takeAnnot} =
  makeError takeAnnot . IllegalHole hole $ mapWrapped annotated

isNotScheme :: (HasPos a, MakeWrapped n) => Annotation n a -> Error
isNotScheme annotated@Annot{takeAnnot} =
  makeError takeAnnot . IsNotScheme $ mapWrapped annotated
