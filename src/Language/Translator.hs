{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Language.Translator
Description : Cmm to llvm translation layer
Maintainer  : jiriklepl@seznam.cz

This module follows the `Language.AST.LRAnalysis` module and translates the AST using its annotations into llvm.
There is no AST-aware module that would follow this module.
-}
module Language.Translator where

import safe Data.Text (Text)
import safe Control.Monad.State.Lazy
import safe Data.Map.Lazy (Map)
import safe qualified Data.Map.Lazy as Map
import safe Data.Foldable
import safe Data.Functor
import safe qualified Data.Text as T

import LLVM.IRBuilder.Instruction
import safe LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import safe qualified LLVM.AST.Name as L
import safe qualified LLVM.AST.Type as L
import safe qualified LLVM.AST.Operand as L
import Text.Megaparsec (SourcePos)
import Control.Applicative
import safe Prettyprinter

import safe Language.AST
import safe Language.AST.Utils
import safe Language.Parser.Utils
import safe Language.AST.LRAnalysis
import safe Language.Pretty()

data TranslState = TranslState
    { variables :: [Map Text L.Operand]
    , translControlFlow :: [(Int, Int)] -- We need the control flow to create the phi nodes
    , translBlockData :: BlockData -- We need the block data to create the phi nodes
    , translCurrentBlock :: Maybe Int
    , translBlocksTable :: Map Int Text -- All GOTOs etc call blocks by their names
    , translErrors :: Int
    , translWarnings :: Int
    }

initTranslState :: TranslState
initTranslState = TranslState -- FIXME: this is just DUMMY
    { variables = mempty
    , translControlFlow = mempty
    , translBlockData = mempty
    , translCurrentBlock = Nothing -- TODO: change to (Just 0) in procedure translation
    , translBlocksTable = mempty
    , translErrors = 0
    , translWarnings = 0
    }

type MonadTranslator m = (MonadIRBuilder m, MonadModuleBuilder m, MonadFix m, MonadState TranslState m {-, MonadIO m -}) -- TODO: solve what with IO

type Exports = [(Text, Int, L.Operand)]

translateName :: HasName n => n -> L.Name
translateName = L.mkName . T.unpack . getName

translateParName :: HasName n => n -> ParameterName
translateParName = (\(L.Name n) -> ParameterName n) . translateName

-- TODO: maybe change the name to `endBlock` or something...
pushVariables :: (MonadTranslator m, Pretty n, HasPos n) => n -> m Exports
pushVariables _ = do
    s@TranslState{variables, translCurrentBlock} <- get
    case translCurrentBlock of
        Just idx -> do
            put s {variables = tail variables, translCurrentBlock = Nothing}
            return $ (\(v, o) -> (v, idx, o)) <$> Map.toList (head variables)
        Nothing -> do
            -- makeMessage mkWarning n "The variables from this block are lost" -- TODO: make clearer
            put s {variables = tail variables, translCurrentBlock = Nothing}
            return mempty

setCurrentBlock :: MonadState TranslState m => Int -> m ()
setCurrentBlock n = do
    modify $ \s -> s {translCurrentBlock = Just n}

class MonadTranslator m => Translate m n b | m n -> b, b -> m where
    translate :: Annot n (SourcePos, BlockAnnot) -> b

instance MonadTranslator m => Translate m Formal (m (L.Type, ParameterName)) where
    translate (Annot formal _) = return . (L.i32,) . translateParName $ formal

instance MonadTranslator m => Translate m Procedure (m L.Operand) where
    translate (Annot _ (_, Unreachable)) = undefined -- TODO: add a nice compile-bug error
    translate (Annot _ (_, NoBlock)) = undefined -- TODO: add a nice compile-bug error
    translate (Annot _ (_, PartOf _)) = undefined -- TODO: add a nice compile-bug error
    translate (Annot (Procedure _ name formals body) (_, Begins blockIdx)) = do
        translFormals <- traverse translate formals
        modify $ \s -> s {translCurrentBlock = Just blockIdx}
        function (translateName name) translFormals L.i32 $
            \pars -> mdo
                let newVars = Map.fromList (zip (getName <$> formals) pars)
                modify (\s -> s{variables = newVars : variables s})
                exports <- (\exports' -> liftA2 (<>) (translate body exports') (pushVariables body)) exports
                modify (\s -> s{variables = tail $ variables s})

instance MonadTranslator m => Translate m Body (Exports -> m Exports) where
    translate (Annot (Body []) _) _ = return mempty
    translate (Annot (Body items) _) exports = fold <$> traverse (`translate` exports) items

instance MonadTranslator m => Translate m BodyItem (Exports -> m Exports) where
    translate (Annot (BodyDecl decl) _) _ = translate decl $> mempty -- TODO ?
    translate (Annot (BodyStackDecl stackDecl) _) _ = translate stackDecl $> mempty -- TODO ?
    translate (Annot (BodyStmt stmt) (_, Begins idx)) exports = do
        bData <- gets translBlockData
        let vars = case idx `Map.lookup` bData of
                    Just xs -> Map.keys $ Map.filter (\(_, _, l) -> l) xs
                    Nothing -> []
        from <- (fst <$>) . filter (\(_, to) -> to == idx) <$> gets translControlFlow
        names <- gets translBlocksTable
        exports' <- pushVariables stmt
        setCurrentBlock idx
        modify (\s -> s{variables = mempty : variables s})
        sequence_
            [do o <- phi $ [
                    let Just (_, _, source) = (\(v', f', _) -> v' == v && f' == f) `find` exports
                    in (source, L.mkName . T.unpack $ names Map.! f)
                    | f <- from]
                modify $ \s -> let (h : t) = variables s in s {variables = Map.insert v o h : t}
            | v <- vars]
        (exports' <>) <$> translate stmt
    translate (Annot (BodyStmt stmt) _) _ = translate stmt

instance MonadTranslator m => Translate m Decl (m ()) where -- TODO: continue from here
    translate _ = return () -- TODO: continue from here

{- |
Guarantees:
- Every IfStmt has two bodies consisting of trivial goto statements
- Every SwitchStmt's arm consists of a trivial goto statement
-}
instance MonadTranslator m => Translate m Stmt (m Exports) where
    translate (Annot EmptyStmt _) = return mempty
    translate (Annot (IfStmt c t (Just e)) _) = do -- TODO: Nothing is compilation pipeline error
        let Just tLab = getTrivialGotoTarget t
            Just eLab = getTrivialGotoTarget e
        c' <- translate c
        condBr c' (L.mkName $ T.unpack tLab) (L.mkName $ T.unpack eLab)
        return mempty
    translate (Annot (SwitchStmt _ _) _) = return mempty -- TODO: this is taxing
    translate (Annot (AssignStmt lvalues exprs) _) = do
        names <- zipWithM translPair lvalues exprs -- TODO: check for duplicates -> error (also, check if modifiable)
        modify $ \s ->
            let (vars : rest) = variables s
            in s {variables = Map.fromList names `Map.union` vars : rest}
        return mempty
        where
            translPair (Annot (LVName n) _) e =
                (getName n, ) <$> translate e
            translPair (Annot LVRef{} _ ) _ = error "not implemented yet" -- TODO: make case for lvref
    translate _ = return mempty -- TODO: remove this
instance MonadTranslator m => Translate m Expr (m L.Operand) where -- TODO: continue from here
    translate _ = return $ int32 42

instance MonadTranslator m => Translate m LValue (m L.Operand) where -- TODO: continue from here

instance MonadTranslator m => Translate m StackDecl (m ()) where -- TODO: continue from here
