{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module CMM.Monomorphize.Error where

import safe Data.Data (Data)
import safe Data.Functor (void)

import safe Prettyprinter (Pretty(pretty), (<+>), list, parens)

import safe CMM.AST.Annot (Annot, Annotation(Annot, takeAnnot), unAnnot)
import safe CMM.AST.Wrap (ASTWrapper, MakeWrapped(makeWrapped))
import safe CMM.Err.Error (Error)
import safe qualified CMM.Err.Error as Error
import safe CMM.Err.IsError (IsError)
import safe CMM.Inference.Preprocess.Elaboration (Elaboration)
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeVar (TypeVar)
import safe CMM.Inference.Unify.Error (UnificationError)
import safe CMM.Monomorphize.Polytypeness (Absurdity, PolyWhat)
import safe CMM.Monomorphize.Schematized
  ( Schematized(ProcedureScheme, StructScheme)
  )
import safe CMM.Parser.ASTError (makeASTError)
import safe CMM.Parser.GetPos (GetPos)

-- | Various errors that can be registered by the monomorphizer (for their explanation, see the pretty instance)
data MonomorphizeError
  = ReachedMaxWaves Int
  | AbsurdType Absurdity (ASTWrapper ())
  | IllegalPolyType PolyWhat (ASTWrapper ())
  | InstantiatesToNothing (ASTWrapper ())
  | IllegalElab Elaboration (ASTWrapper ())
  | IllegalScheme (Schematized ()) (ASTWrapper ())
  | NoInstance TypeVar TypeVar (ASTWrapper ())
  | IsNotScheme (ASTWrapper ())
  | CannotInstantiate Type Type [UnificationError] (ASTWrapper ())
  | Ambiguity [Elaboration] (ASTWrapper ())
  deriving (Show, Eq, IsError, Data)

instance Pretty MonomorphizeError where
  pretty =
    \case
      ReachedMaxWaves int ->
        "Reached the maximum number of monomorphization waves" <+>
        parens (pretty int)
      AbsurdType absurdity node ->
        pretty node <+> "has absurd type:" <+> pretty absurdity
      IllegalPolyType polyWhat node ->
        pretty node <+>
        "is poly-typed in the context that requires monotypes:" <+>
        pretty polyWhat
      InstantiatesToNothing node -> pretty node <+> "instantiates into nothing"
      IllegalElab hole node ->
        pretty node <+>
        "has an illegal elaboration" <+> parens (pretty hole) <> report
      IllegalScheme schematized node ->
        pretty node <+>
        case schematized of
          ProcedureScheme func ->
            "Is required to be a structure, but has been given a function:" <+>
            pretty func
          StructScheme struct ->
            "Is required to be a function, but has been given a structure:" <+>
            pretty struct
      NoInstance scheme inst node ->
        pretty node <+>
        "with a scheme" <+>
        pretty scheme <+> "failed to instantiate into" <+> pretty inst
      IsNotScheme node ->
        "There is no scheme registered for" <+> pretty node <> report
      CannotInstantiate scheme inst unifErrors node ->
        pretty node <+>
        "cannot be instantiated from the type" <+>
        pretty scheme <+>
        "to the type" <+>
        pretty inst <+> "due to" <+> list (pretty <$> unifErrors)
      Ambiguity holes node ->
        case holes of
          [_] ->
            pretty node <+>
            " was not instantiated due to broken contract in monomorphization"
          [] ->
            pretty node <+>
            " cannot be instantiated, there are no suitable schemes"
          _ ->
            pretty node <+>
            " cannot be instantiated due to ambiguity between the following types:" <+>
            list (pretty <$> holes)
    where
      report = ", report bug in inference preprocessing"

-- | a shortcut for making AST errors of error severity
makeError :: GetPos n => n -> MonomorphizeError -> Error
makeError n = Error.makeError . makeASTError n

-- | strips annotations from the given node and wraps it in the `ASTWrapper`
voidWrapped :: MakeWrapped n => Annot n a -> ASTWrapper ()
voidWrapped = void . makeWrapped . unAnnot

-- | helper for creating AST error containing `InstantiatesToNothing`
illegalNothing :: (GetPos a, MakeWrapped n) => Annot n a -> Error
illegalNothing annotated@Annot {takeAnnot} =
  makeError takeAnnot . InstantiatesToNothing $ voidWrapped annotated

-- | helper for creating AST error containing `AbsurdType`
absurdType :: (GetPos a, MakeWrapped n) => Absurdity -> Annotation n a -> Error
absurdType absurdity annotated@Annot {takeAnnot} =
  makeError takeAnnot . AbsurdType absurdity $ voidWrapped annotated

-- | helper for creating AST error containing `IllegalPolyType`
illegalPolyType ::
     (GetPos a, MakeWrapped n) => PolyWhat -> Annotation n a -> Error
illegalPolyType absurdity annotated@Annot {takeAnnot} =
  makeError takeAnnot . IllegalPolyType absurdity $ voidWrapped annotated

-- | helper for creating AST error containing `IllegalElab`
illegalHole ::
     (GetPos a, MakeWrapped n) => Elaboration -> Annotation n a -> Error
illegalHole hole annotated@Annot {takeAnnot} =
  makeError takeAnnot . IllegalElab hole $ voidWrapped annotated

-- | helper for creating AST error containing `IsNotScheme`
isNotScheme :: (GetPos a, MakeWrapped n) => Annotation n a -> Error
isNotScheme annotated@Annot {takeAnnot} =
  makeError takeAnnot . IsNotScheme $ voidWrapped annotated
