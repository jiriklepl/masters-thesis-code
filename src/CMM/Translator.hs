{-# LANGUAGE Trustworthy #-}

{-|
Module      : CMM.Translator
Description : Cmm to llvm translation layer
Maintainer  : jiriklepl@seznam.cz

This module follows the `CMM.AST.LRAnalysis` module and translates the AST using its annotations into llvm.
There is no AST-aware module that would follow this module.
-}
module CMM.Translator where

import safe Control.Applicative
import safe Control.Monad.State
import safe Data.Char
import safe Data.Foldable
import safe Data.Function
import safe Data.Functor
import safe qualified Data.Map as Map
import safe Data.String
import safe Data.Text (Text)
import safe qualified Data.Text as T

import safe Control.Lens.Getter
import safe Control.Lens.Setter
import safe Control.Lens.Tuple

import safe qualified LLVM.AST.Constant as L
import safe qualified LLVM.AST.IntegerPredicate as L
import safe qualified LLVM.AST.Name as L
import safe qualified LLVM.AST.Operand as L
import safe qualified LLVM.AST.Instruction as LI
import safe qualified LLVM.AST.Type as L
import qualified LLVM.AST.Typed as L
import qualified LLVM.IRBuilder.Constant as L
import qualified LLVM.IRBuilder.Instruction as L
import qualified LLVM.IRBuilder.Module as L
import qualified LLVM.IRBuilder.Monad as L

import safe CMM.AST
import safe CMM.AST.Annot
import safe CMM.AST.BlockAnnot
import safe CMM.AST.GetName
import safe CMM.AST.Utils
import safe CMM.Lens
import safe CMM.Parser.HasPos
import safe CMM.Pretty ()
import safe CMM.Translator.State
import Debug.Trace
import Data.Tuple.Extra (first)
import Prettyprinter
import CMM.Data.Function
import qualified Data.Vector as V
import Data.Vector (Vector)
import safe Data.Map (Map)
import CMM.Data.Tuple
import Data.Maybe
import qualified LLVM.IRBuilder.Internal.SnocList as L
import Data.Generics

type MonadTranslator m
   = ( L.MonadIRBuilder m
     , L.MonadModuleBuilder m
     , MonadFix m
     , MonadState TranslState m
      )

type OutVar = (Text, L.Operand)

type OutVars = [OutVar]

translateName :: GetName n => n -> L.Name
translateName = L.mkName . T.unpack . getName

translateParName :: GetName n => n -> L.ParameterName
translateParName = (\(L.Name n) -> L.ParameterName n) . translateName

setCurrentBlock :: MonadState TranslState m => Int -> m ()
setCurrentBlock n = currentBlock ?= n

class (HasBlockAnnot a, HasPos a, MonadTranslator m) =>
      Translate m n a b
  | m n -> b
  , b -> m
  where
  translate :: Annot n a -> b

instance (HasBlockAnnot a, HasPos a, MonadTranslator m) =>
         Translate m Formal a (m (L.Type, L.ParameterName)) where
  translate (Annot formal _) = return . (L.i32, ) . translateParName $ formal

instance (HasBlockAnnot a, HasPos a, MonadTranslator m) =>
         Translate m TopLevel a (m L.Operand) where
  translate (topLevel `Annot` _) = case topLevel of
    TopSection sl ans -> undefined
    TopDecl an -> undefined
    TopProcedure procedure -> translate procedure
    TopClass an -> undefined
    TopInstance an -> undefined
    TopStruct an -> undefined


instance (HasBlockAnnot a, HasPos a, MonadTranslator m) =>
         Translate m Procedure a (m L.Operand) where
  translate (Annot (Procedure (Annot (ProcedureHeader _ name formals _) _) body) annot) =
    go $ getBlockAnnot annot
    where
      go =
        \case
          Begins idx -> do
            formals' <- traverse translate formals
            f <- L.function (translateName name) formals' L.i32 $ \pars ->
               do rename .= mempty
                  blockName <- uses blocksTable (Map.! idx)
                  L.emitBlockStart . fromString $ T.unpack blockName
                  rename' <- translate body idx . Map.fromList $ zip formalNames pars
                  rename .= rename'
                  L.modifyBlock $ \bb -> bb
                    { L.partialBlockInstrs = L.SnocList $ L.unSnocList (L.partialBlockInstrs bb) & renameGeneric rename'
                    , L.partialBlockTerm = L.partialBlockTerm bb & renameGeneric rename'
                    }
                  L.liftIRState $ modify $ \s -> s
                    { L.builderBlocks =  L.SnocList $ L.unSnocList (L.builderBlocks s) & renameGeneric rename'
                    , L.builderSupply = fromInteger $ toInteger (L.builderSupply s) - toInteger (Map.size rename')
                    }
            traceShowM f
            return f
          _ -> undefined -- TODO: add nice error message for completeness
        where formalNames = getName <$> formals

applyRename :: Ord k => Map k k -> k -> Maybe k
applyRename subst name = Map.lookup name subst

renameGeneric :: Data d => Map L.Name L.Name -> d -> d
renameGeneric subst = go
  where
    go :: Data d => d -> d
    go = gmapT go `extT` nameCase
    nameCase (name :: L.Name) = case applyRename subst name of
      Just renamed -> renamed
      Nothing -> case name of
        L.UnName int -> L.UnName . fromInteger $ toInteger int  - toInteger (Map.size (Map.filterWithKey(\k _ -> k < name) subst))
        _ -> name

instance (HasBlockAnnot a, HasPos a, MonadTranslator m) =>
         Translate m Body a (Int -> Map Text L.Operand ->m (Map L.Name L.Name)) where
  translate (Annot (Body []) _) _ _ = return mempty
  translate (Annot (Body items) _) zero newVars = do
    off <- uses offSets $ fromJust . Map.lookup zero
    blockData' <- uses blockData $ Map.filterWithKey $ \k _ -> k >= zero && k < zero + off
    let vars = blockData' <&> Map.keys . Map.filter ((^. _3) `fOr` (^. _2))
    exports <- Map.elems <$> traverse (traverse $ \v -> (v,) . L.LocalReference L.i32  <$> L.freshName (fromString $ T.unpack v)) (Map.delete zero vars)
    newVars' <- translateMany header newVars
    nVars'' <- traverse (translateBlock zero (Map.toList newVars') $ V.fromList exports) (V.fromList blocks)
    let
      unOperand (L.LocalReference _ name) = name
      unOperand _ = undefined
    return . Map.fromList $ zip (unOperand . snd <$> concat (V.toList nVars'')) (unOperand . snd <$> concat exports)
    where
      (header, body) = tillNext items -- header = the rest of the entry block of the procedure
      blocks = splitBlocks body
      splitBlocks [] =[]
      splitBlocks (item:items')
        | Begins idx <- getBlockAnnot item = (item:part, idx) : splitBlocks rest
        where (part, rest) = tillNext items'
      splitBlocks _ = undefined
      tillNext items'@(item:rest)
        | Begins {} <- annot = ([], items')
        | Unreachable {} <- annot = tillNext rest
        | NoBlock {} <- annot = tillNext rest
        where annot = getBlockAnnot item
      tillNext (item:items') = first (item:) $ tillNext items'
      tillNext [] = ([], [])

translateBlock :: (MonadTranslator m, HasBlockAnnot a, HasPos a) => Int ->  OutVars -> Vector OutVars -> ([Annot BodyItem a], Int) -> m OutVars
translateBlock zero entry exports (~(item:items), idx) = do
  vars <- uses blockData (idx `Map.lookup`) <&> maybe [] (Map.keys . Map.filter (^. _3))
  from <- uses controlFlow $ (fst <$>) . filter (\(_, t) -> t == idx)
  names <- use blocksTable
  nVars <- translate item mempty
  nVars' <- Map.fromList <$> sequence
    [ do
        let sourceList =
              [ let source =
                      fromJust $ find (\(v', _) -> v' == v) $ if f == zero
                        then entry
                        else exports V.! (f - zero - 1)
                    mkRecord s = (s, L.mkName . T.unpack $ names Map.! f)
                in mkRecord $ source ^. _2
              | f <- from
              ]
        o <- L.phi sourceList
        return (v, o)
    | v <- vars
    ]
  Map.toList <$> translateMany items (nVars' `Map.union` nVars)

translateMany :: (Translate m1 n a1 (a2 -> m2 a2), Monad m2) => [Annot n a1] -> a2 -> m2 a2
translateMany [] vars = return vars
translateMany (item:items) vars = translate item vars >>= translateMany items

instance (HasBlockAnnot a, HasPos a, MonadTranslator m) =>
         Translate m BodyItem a (Map Text L.Operand -> m (Map Text L.Operand)) where
  translate (Annot (BodyDecl decl) _) = translate decl -- TODO ?
  translate (Annot (BodyStackDecl stackDecl) _) =
    translate stackDecl -- TODO ?
  translate (Annot (BodyStmt stmt) _) = translate stmt

instance (HasBlockAnnot a, HasPos a, MonadTranslator m) =>
         Translate m Decl a (Map Text L.Operand -> m (Map Text L.Operand)) -- TODO: continue from here
                                                               where
  translate _ = return -- TODO: continue from here

{- |
Guarantees:
- Every IfStmt has two bodies consisting of trivial goto statements
- Every SwitchStmt's arm consists of a trivial goto statement
-}
instance (HasBlockAnnot a, HasPos a, MonadTranslator m) =>
         Translate m Unit a (m ()) where
  translate (Unit topLevels `Annot` _) =
    traverse_ translate topLevels

instance (HasBlockAnnot a, HasPos a, MonadTranslator m) =>
         Translate m Stmt a (Map Text L.Operand -> m (Map Text L.Operand)) where
  translate (stmt `Annot` annot) vars = case stmt of
    EmptyStmt -> return vars
    IfStmt {}
      | IfStmt c t (Just e) <- stmt -> do
          let Just tLab = getTrivialGotoTarget t
              Just eLab = getTrivialGotoTarget e
          c' <- translate c vars
          L.condBr c' (L.mkName $ T.unpack tLab) (L.mkName $ T.unpack eLab)
          return vars
      | otherwise -> error "internal inconsistency"
    SwitchStmt {} -> undefined -- TODO: this is taxing
    SpanStmt {} -> undefined
    AssignStmt lvalues exprs -> do
      assigns <- zipWithM translPair lvalues exprs -- TODO: check for duplicates -> error (also, check if modifiable)
      let vars' = Map.fromList assigns <> vars -- TODO: traverse all frames (assignments to global registers)
      return vars'
      where
        translPair (Annot (LVName n) _) e = (getName n, ) <$> translate e vars
        translPair (Annot LVRef {} _) _ = error "not implemented yet" -- TODO: make case for lvref
    PrimOpStmt {} -> undefined
    CallStmt {}
      | CallStmt rets _ expr actuals Nothing [] <- stmt -> undefined
      | otherwise -> undefined
    JumpStmt {} -> undefined
    ReturnStmt {}
      | ReturnStmt Nothing Nothing [actual] <- stmt -> do
        ret <- translate actual vars
        L.ret ret $> vars
      | otherwise -> undefined
    LabelStmt name -> (L.emitBlockStart . fromString . T.unpack . getName) name $> vars
    ContStmt na ans -> undefined
    GotoStmt {}
      | GotoStmt _ Nothing <- stmt ->do
            let Just lab = getTrivialGotoTarget stmt -- TODO: is this safe?
            L.br (L.mkName $ T.unpack lab)
            return vars
      | otherwise -> undefined
    CutToStmt an ans ans' -> undefined
instance (HasBlockAnnot a, HasPos a, MonadTranslator m) =>
         Translate m Actual a (Map Text L.Operand -> m L.Operand) where
  translate (Annot (Actual Nothing expr) _) vars = translate expr vars
  translate _ vars = undefined

-- Source: https://www.cs.tufts.edu/~nr/c--/extern/man2.pdf (7.4)
instance (HasBlockAnnot a, HasPos a, MonadTranslator m) =>
         Translate m Expr a (Map Text L.Operand -> m L.Operand) where
  translate (Annot (LitExpr lit Nothing) _) _ = translate lit
  translate (Annot (ParExpr expr) _) vars = translate expr vars
  translate (Annot (LVExpr lvalue) _) vars = translate lvalue vars
  translate (Annot (BinOpExpr o l r) _) vars = do
    l' <- translate l vars
    r' <- translate r vars
    (r' &) . (l' &) $
      case o of
        AddOp -> L.add
        SubOp -> L.sub
        MulOp -> L.mul
        DivOp -> L.udiv
        ModOp -> L.urem
        AndOp -> L.and
        OrOp -> L.or
        XorOp -> L.xor
        ShLOp -> L.shl
        ShROp -> L.lshr
            -- https://llvm.org/docs/LangRef.html#icmp-instruction
        EqOp -> L.icmp L.EQ
        NeqOp -> L.icmp L.NE
        GtOp -> L.icmp L.UGT
        LtOp -> L.icmp L.ULT
        GeOp -> L.icmp L.UGE
        LeOp -> L.icmp L.ULE
  translate (Annot (ComExpr expr) _) vars = do
    expr' <- translate expr vars
    L.typeOf expr' >>= \case
      Right (L.IntegerType bits) ->
        L.xor expr' . L.ConstantOperand $ L.Int bits (-1)
      _ -> error "Cannot create a binary complement to a non-int"
  translate (Annot (NegExpr expr) _) vars = translate expr vars >>= L.icmp L.EQ (L.bit 0)

instance (HasBlockAnnot a, HasPos a, MonadTranslator m) =>
         Translate m Lit a (m L.Operand) where
  translate (Annot (LitInt int) _) = return . L.int32 $ toInteger int -- TODO: discuss this later
  translate (Annot (LitFloat float) _) = return $ L.single float
  translate (Annot (LitChar char) _) = return . L.int8 . toInteger $ ord char

-- TODO: continue from here
instance (HasBlockAnnot a, HasPos a, MonadTranslator m) =>
         Translate m LValue a (Map Text L.Operand -> m L.Operand) where
  translate (Annot (LVName n) _) vars = do
    maybe (error $"Variable not found " <> show n) return (getName n `Map.lookup` vars) -- TODO: traverse all frames (accessing global registers; also, accessing non-variables); remove the error
  translate (Annot LVRef {} _) vars = do
    error "references not yet implemented" -- TODO: implement lvref

-- TODO: continue from here
instance (HasBlockAnnot a, HasPos a, MonadTranslator m) =>
         Translate m StackDecl a (Map Text L.Operand -> m (Map Text L.Operand)) where
  translate = undefined
