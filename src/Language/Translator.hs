{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Language.Translator
Description : Cmm to llvm translation layer
Maintainer  : jiriklepl@seznam.cz

This module follows the `Language.AST.LRAnalysis` module and translates the AST using its annotations into llvm.
There is no AST-aware module that would follow this module.
-}
module Language.Translator where

import safe Data.Text (Text)
import safe Data.Function
import safe Control.Monad.State.Lazy
import safe Control.Lens.Getter
import safe Control.Lens.Setter
import safe Control.Lens.Tuple
import safe Control.Lens.TH
import safe Data.Map.Lazy (Map)
import safe qualified Data.Map.Lazy as Map
import safe Data.Foldable
import safe Data.Functor
import safe qualified Data.Text as T
import safe Data.Char

import safe qualified LLVM.IRBuilder.Instruction as L
import safe LLVM.IRBuilder.Constant
import safe LLVM.IRBuilder.Module
import safe qualified LLVM.IRBuilder.Monad as L
import safe qualified LLVM.AST.Name as L
import safe qualified LLVM.AST.Type as L
import safe qualified LLVM.AST.Operand as L
import safe qualified LLVM.AST.IntegerPredicate as L
import safe Text.Megaparsec (SourcePos)
import safe Control.Applicative
import safe Prettyprinter

import safe Language.AST
import safe Language.AST.Utils
import safe Language.Parser.Utils
import safe Language.AST.BlockAnnot
import safe Language.Pretty()

data TranslState = TranslState
    { _variables :: [Map Text L.Operand]
    , _controlFlow :: [(Int, Int)] -- We need the control flow to create the phi nodes
    , _blockData :: BlockData -- We need the block data to create the phi nodes
    , _currentBlock :: Maybe Int
    , _blocksTable :: Map Int Text -- All GOTOs etc call blocks by their names
    , _errors :: Int
    , _warnings :: Int
    }

makeLenses ''TranslState

initTranslState :: TranslState
initTranslState = TranslState -- FIXME: this is just DUMMY
    { _variables = mempty
    , _controlFlow = mempty
    , _blockData = mempty
    , _currentBlock = Nothing -- TODO: change to (Just 0) in procedure translation
    , _blocksTable = mempty
    , _errors = 0
    , _warnings = 0
    }

type MonadTranslator m = (L.MonadIRBuilder m, MonadModuleBuilder m, MonadFix m, MonadState TranslState m {-, MonadIO m -}) -- TODO: solve what with IO

type OutVar = (Text, Int, L.Operand)
type OutVars = [OutVar]

translateName :: HasName n => n -> L.Name
translateName = L.mkName . T.unpack . getName

translateParName :: HasName n => n -> ParameterName
translateParName = (\(L.Name n) -> ParameterName n) . translateName

infix 4 `exchange`
exchange :: MonadState s m => (forall f. Functor f => (a -> f a) -> s -> f s) -> a -> m a
l `exchange` b = use l <* (l .= b)

-- TODO: maybe change the name to `endBlock` or something...
pushVariables :: (MonadTranslator m, Pretty n, HasPos n) => n -> m OutVars
pushVariables _ = do
    currentBlock `exchange` Nothing >>= \case
        Just idx -> do
            ~(h : t) <- use variables
            variables .= t
            return $ (\(v, o) -> (v, idx, o)) <$> Map.toList h
        Nothing -> do
            -- makeMessage mkWarning n "The variables from this block are lost" -- TODO: make clearer
            variables %= tail
            return mempty

setCurrentBlock :: MonadState TranslState m => Int -> m ()
setCurrentBlock n = currentBlock ?= n

-- TODO: maybe move somewhere else
class HasBlockAnnot a where
    getBlockAnnot :: a -> BlockAnnot

instance HasBlockAnnot BlockAnnot where
    getBlockAnnot = id

instance HasBlockAnnot (SourcePos, BlockAnnot) where
    getBlockAnnot = snd

class MonadTranslator m => Translate m n b | m n -> b, b -> m where
    translate :: Annot n (SourcePos, BlockAnnot) -> b

instance MonadTranslator m => Translate m Formal (m (L.Type, ParameterName)) where
    translate (Annot formal _) = return . (L.i32,) . translateParName $ formal

instance MonadTranslator m => Translate m Procedure (m L.Operand) where
    translate (Annot (Procedure _ name formals body) annot) =
        go $ getBlockAnnot annot
        where
            go = \case
                Begins blockIdx -> do
                    formals' <- traverse translate formals
                    currentBlock ?= blockIdx
                    function (translateName name) formals' L.i32 $
                        \pars -> mdo
                            let newVars = Map.fromList (zip (getName <$> formals) pars)
                            variables %= (newVars :)
                            exports <- (\exports' -> liftA2 (<>) (translate body exports') (pushVariables body)) exports
                            variables %= tail
                _ -> undefined -- TODO: add nice error message

instance MonadTranslator m => Translate m Body (OutVars -> m OutVars) where
    translate (Annot (Body []) _) _ = return mempty
    translate (Annot (Body items) _) exports = fold <$> traverse (`translate` exports) items

instance MonadTranslator m => Translate m BodyItem (OutVars -> m OutVars) where
    translate (Annot (BodyDecl decl) _) _ = translate decl $> mempty -- TODO ?
    translate (Annot (BodyStackDecl stackDecl) _) _ = translate stackDecl $> mempty -- TODO ?
    translate (Annot (BodyStmt stmt) annot) exports =
        go $ getBlockAnnot annot
        where
            go = \case
                Begins idx -> do
                    vars <- uses blockData (idx `Map.lookup`) <&>
                        maybe [] (Map.keys . Map.filter (^._3))
                    from <- uses controlFlow $ (fst <$>) . filter (\(_, t) -> t == idx)
                    names <- use blocksTable
                    exports' <- pushVariables stmt
                    setCurrentBlock idx
                    variables %= (mempty :)
                    sequence_
                        [do o <- L.phi $ [
                                let Just source = (\(v', f', _) -> v' == v && f' == f) `find` exports
                                in (source^._3, L.mkName . T.unpack $ names Map.! f)
                                | f <- from]
                            ~(h : t) <- use variables
                            variables .= Map.insert v o h : t
                        | v <- vars]
                    (exports' <>) <$> translate stmt
                _ -> translate stmt

instance MonadTranslator m => Translate m Decl (m ()) where -- TODO: continue from here
    translate _ = return () -- TODO: continue from here

{- |
Guarantees:
- Every IfStmt has two bodies consisting of trivial goto statements
- Every SwitchStmt's arm consists of a trivial goto statement
-}
instance MonadTranslator m => Translate m Stmt (m OutVars) where
    translate (Annot EmptyStmt _) = return mempty
    translate (Annot (IfStmt c t (Just e)) _) = do -- TODO: Nothing is compilation pipeline error
        let Just tLab = getTrivialGotoTarget t
            Just eLab = getTrivialGotoTarget e
        c' <- translate c
        L.condBr c' (L.mkName $ T.unpack tLab) (L.mkName $ T.unpack eLab)
        return mempty
    translate (Annot (SwitchStmt _ _) _) = return mempty -- TODO: this is taxing
    translate (Annot (AssignStmt lvalues exprs) _) = do
        assigns <- zipWithM translPair lvalues exprs -- TODO: check for duplicates -> error (also, check if modifiable)
        ~(vars : rest) <- use variables
        variables .= Map.fromList assigns <> vars : rest -- TODO: traverse all frames (assignments to global registers)
        return mempty
        where
            translPair (Annot (LVName n) _) e =
                (getName n, ) <$> translate e
            translPair (Annot LVRef{} _ ) _ = error "not implemented yet" -- TODO: make case for lvref
    translate _ = return mempty -- TODO: remove this

-- Source: https://www.cs.tufts.edu/~nr/c--/extern/man2.pdf (7.4)
instance MonadTranslator m => Translate m Expr (m L.Operand) where
    translate (Annot (LitExpr lit Nothing) _) = translate lit
    translate (Annot (ParExpr expr) _) = translate expr
    translate (Annot (LVExpr lvalue) _) = translate lvalue
    translate (Annot (BinOpExpr o l r) _) = do
        l' <- translate l
        r' <- translate r
        (r' &) . (l' &) $ case o of
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


instance MonadTranslator m => Translate m Lit (m L.Operand) where
    translate (Annot (LitInt int) _) = return . int32 $ toInteger int -- TODO: discuss this later
    translate (Annot (LitFloat float) _) = return $ single float
    translate (Annot (LitChar char) _) = return . int8 . toInteger $ ord char

instance MonadTranslator m => Translate m LValue (m L.Operand) where -- TODO: continue from here
    translate (Annot (LVName n) _) = do
        ~(h : _) <- use variables
        maybe (error "Variable not found") return (getName n `Map.lookup` h) -- TODO: traverse all frames (accessing global registers; also, accessing non-variables); remove the error
    translate (Annot LVRef{} _ ) = do
        error "references not yet implemented" -- TODO: implement lvref

instance MonadTranslator m => Translate m StackDecl (m ()) where -- TODO: continue from here
