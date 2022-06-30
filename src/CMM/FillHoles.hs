{-# LANGUAGE Safe #-}

module CMM.FillHoles where


import safe qualified CMM.Inference.State as State
import safe qualified CMM.Inference
import safe qualified CMM.AST as AST

import safe CMM.Inference.State (Inferencer)
import CMM.AST.Annot
import CMM.Inference.Preprocess.TypeHole
import Data.Data
import Data.Generics.Aliases
import CMM.Parser.HasPos
import CMM.AST.BlockAnnot
import CMM.Inference.TypeVar
import CMM.Inference.Type
import CMM.Inference.TypeCompl
import Control.Lens

type FillAnnot = ((SourcePos, BlockAnnot), TypeHole)

class FillHoles n where
  fillHoles :: Data (n FillAnnot) => Annot n FillAnnot -> Inferencer (Annot n FillAnnot)

typeFromHoled :: HasTypeHole a => a -> Inferencer (AST.Type a)
typeFromHoled holed =
  State.getTyping (toTypeVar $ getTypeHole holed) <&> translType holed

translType :: a -> Type -> AST.Type a
translType holed = \case
    ErrorType txt -> undefined
    VarType tv -> undefined
    ComplType tc -> case tc of
      TupleType tys -> undefined
      FunctionType tys ty -> undefined
      AppType ty ty' -> undefined
      AddrType t -> AST.TPtr $ withAnnot holed $ translType holed t
      ConstType txt _ _ -> AST.TName $ AST.Name txt
      StringType -> undefined
      String16Type -> undefined
      LabelType -> undefined
      TBitsType n -> AST.TBits n
      BoolType -> undefined
      VoidType -> undefined

instance Typeable n => FillHoles n where
  fillHoles = go
    where
      go :: Data d => d -> Inferencer d
      go = gmapM go `extM` registersCase `extM` formalCase `extM` semiFormalCase
      registersCase (AST.Registers mKind (_ `Annot` b) nameStrs `Annot` (a :: FillAnnot)) = do
        t' <- typeFromHoled b
        return (AST.Registers mKind (withAnnot b t') nameStrs `Annot` a)
      formalCase (AST.Formal mKind bool (_ `Annot` b) name `Annot` (a :: FillAnnot)) = do
        t' <- typeFromHoled b
        return (AST.Formal mKind bool (withAnnot b t') name `Annot` a)
      semiFormalCase (AST.SemiFormal mKind (_ `Annot` b) `Annot` (a :: FillAnnot)) = do
        t' <- typeFromHoled b
        return (AST.SemiFormal mKind (withAnnot b t') `Annot` a)

