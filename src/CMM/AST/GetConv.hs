{-# LANGUAGE Safe #-}

module CMM.AST.GetConv where

import safe CMM.AST
  ( BodyItem(BodyStmt)
  , Conv
  , Procedure(Procedure)
  , ProcedureDecl(ProcedureDecl)
  , ProcedureHeader(ProcedureHeader)
  , Section(SecProcedure)
  , Stmt(CallStmt, JumpStmt, ReturnStmt)
  , TopLevel(TopProcedure)
  )
import safe CMM.AST.Annot (Annot, unAnnot)

-- | Gets the call convention specialization from the given node
class GetConv n where
  getConv :: n -> Maybe Conv

instance GetConv (n a) => GetConv (Annot n a) where
  getConv = getConv . unAnnot

instance GetConv (BodyItem a) where
  getConv =
    \case
      BodyStmt stmt -> getConv stmt
      _ -> Nothing

instance GetConv Conv where
  getConv conv = Just conv

instance GetConv (Procedure a) where
  getConv =
    \case
      Procedure header _ -> getConv header

instance GetConv (ProcedureDecl a) where
  getConv =
    \case
      ProcedureDecl header -> getConv header

instance GetConv (ProcedureHeader a) where
  getConv =
    \case
      ProcedureHeader mConv _ _ _ -> mConv

instance GetConv (Stmt a) where
  getConv =
    \case
      CallStmt _ mConv _ _ _ _ -> mConv
      JumpStmt mConv _ _ _ -> mConv
      ReturnStmt mConv _ _ -> mConv
      _ -> Nothing

instance GetConv (TopLevel a) where
  getConv =
    \case
      TopProcedure procedure -> getConv procedure
      _ -> Nothing

instance GetConv (Section a) where
  getConv =
    \case
      SecProcedure procedure -> getConv procedure
      _ -> Nothing
