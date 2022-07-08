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

import safe qualified LLVM.AST.Constant as LC
import safe qualified LLVM.AST.IntegerPredicate as L
import safe qualified LLVM.AST.Name as L
import safe qualified LLVM.AST.Operand as LO
import safe qualified LLVM.AST.Type as LT
import qualified LLVM.AST.Typed as L
import qualified LLVM.IRBuilder.Constant as LC
import qualified LLVM.IRBuilder.Instruction as L
import qualified LLVM.IRBuilder.Module as L
import qualified LLVM.IRBuilder.Monad as L

import safe CMM.AST
import safe CMM.AST.Annot
import safe CMM.AST.BlockAnnot
import safe CMM.AST.GetName
import safe CMM.AST.Utils
import safe CMM.Parser.GetPos
import safe CMM.Pretty ()
import safe CMM.Translator.State
import safe Data.Tuple.Extra (first, swap)
import safe CMM.Data.Function
import safe Data.Map (Map)
import safe Data.Maybe
import safe qualified LLVM.IRBuilder.Internal.SnocList as L
import Data.Generics
import safe CMM.Inference.State
import safe qualified Data.Set as Set
import safe Data.Set (Set)
import safe CMM.TypeMiner
import safe qualified CMM.Inference.Preprocess.TypeHole as Ty
import safe CMM.Inference.Preprocess.TypeHole
import safe Data.List (sortOn)
import safe LLVM.Prelude (Word32)
import safe qualified Data.List as List
import CMM.Utils

type MonadTranslator m
   = ( L.MonadIRBuilder m
     , L.MonadModuleBuilder m
     , MonadFix m
     , MonadState TranslState m
      )

type AnnotAssumps a
   = ( HasBlockAnnot a
     , GetPos a
     , HasTypeHole a
     , Data a
     )

type TranslAssumps a m = (AnnotAssumps a, MonadTranslator m)

type OutVar = (Text, LO.Operand)

type OutVars = [OutVar]

translateName :: GetName n => n -> L.Name
translateName = L.mkName . T.unpack . getName

getIntWidth :: LT.Type -> Word32
getIntWidth = \case
  LT.IntegerType int -> int
  _ -> error "the operand does not have integral type"

translateParName :: GetName n => n -> L.ParameterName
translateParName = (\(L.Name n) -> L.ParameterName n) . translateName

setCurrentBlock :: MonadState TranslState m => Int -> m ()
setCurrentBlock n = currentBlock ?= n

class TranslAssumps a m =>
      Translate m n a b
  | m n -> b
  , b -> m
  where
  translate :: Annot n a -> b

instance TranslAssumps a m =>
         Translate m Formal a (m (LT.Type, L.ParameterName)) where
  translate (Formal _ True _ _ `Annot` _) = undefined
  translate (Formal _ _ type' name `Annot` _) = do
    type'' <- getType type'
    return . (type'', ) $ translateParName name

instance TranslAssumps a m =>
         Translate m TopLevel a (m ()) where
  translate (topLevel `Annot` _) = case topLevel of
    TopSection {} -> undefined
    TopDecl {} -> undefined
    TopProcedure procedure -> void $ translate procedure
    TopClass {} -> undefined
    TopInstance {} -> undefined
    TopStruct struct -> do
      structs' <- use structs
      struct' <- runInferencer $ mineStruct structs' struct
      structs %= uncurry Map.insert struct'

getType :: (MonadTranslator m, HasTypeHole a, HasCallStack) => a -> m LT.Type
getType annot = do
  structs' <- use structs
  runInferencer $ mineTypeHoled structs' annot

getStructName :: (MonadTranslator m, HasTypeHole a) => a -> m Text
getStructName annot =
  runInferencer $ mineStructName annot

instance TranslAssumps a m =>
         Translate m Procedure a (m LO.Operand) where
  translate (Procedure (Annot (ProcedureHeader _ name formals _) _) body `Annot` annot) =
    go $ getBlockAnnot annot
    where
      go =
        \case
          Begins idx -> do
            formals' <- traverse translate formals
            type' <- getType annot
            L.function (translateName name) formals' (LT.resultType type') $ \pars ->
               do clearTranslState
                  blockName <- uses blocksTable (Map.! idx)
                  L.emitBlockStart . fromString $ T.unpack blockName
                  rename' <- translate body idx . Map.fromList $ zip formalNames pars
                  L.modifyBlock $ \bb -> bb
                    { L.partialBlockInstrs = L.SnocList $ L.unSnocList (L.partialBlockInstrs bb) & renameGeneric rename'
                    , L.partialBlockTerm = L.partialBlockTerm bb & renameGeneric rename'
                    }
                  L.liftIRState $ modify $ \s -> s
                    { L.builderBlocks =  L.SnocList $ L.unSnocList (L.builderBlocks s) & renameGeneric rename'
                    , L.builderSupply = fromInteger $ toInteger (L.builderSupply s) - toInteger (Map.size rename')
                    }
          _ -> undefined -- TODO: add nice error message for completeness
        where formalNames = getName <$> formals

applyRename :: Ord k => Map k k -> k -> Maybe k
applyRename subst name = Map.lookup name subst

renameGeneric :: Data d => Map LO.Operand LO.Operand -> d -> d
renameGeneric subst = go
  where
    go :: Data d => d -> d
    go = gmapT go `extT` nameCase
    nameCase (name :: LO.Operand) = case applyRename subst name of
      Just renamed -> renamed
      _ -> name

collectVarTypes :: (Data (n a), Data a, HasTypeHole a) => Set Text -> Annot n a -> Map Text Ty.TypeHole
collectVarTypes names (n `Annot` (_ :: a)) = Map.fromList $ collectNames names (Proxy :: Proxy a) n

collectNames :: (Data d, Data a, HasTypeHole a) => Set Text -> Proxy a -> d -> [(Text, Ty.TypeHole)]
collectNames names (proxy :: Proxy a) = (concat  . gmapQ (collectNames names proxy)) `extQ` lvCase
  where
    lvCase ((LVName (Name name)) `Annot` (a :: a))
      | name `Set.member` names = [(name, getTypeHole a)]
    lvCase lValue = concat $ gmapQ (collectNames names proxy) lValue

runInferencer :: MonadTranslator m => Inferencer a -> m a
runInferencer action =
  uses inferencer $ evalState action

zip' = zipWith' (,)

zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs
zipWith' _ [] [] = []
zipWith' _ _ _ = undefined

instance TranslAssumps a m =>
         Translate m Body a (Int -> Map Text LO.Operand -> m (Map LO.Operand LO.Operand)) where
  translate (Annot (Body []) _) _ _ = return mempty
  translate bodyNode@(Annot (Body items) _) zero newVars = do
    off <- uses offSets $ fromJust . Map.lookup zero
    blockData' <- uses blockData $ Map.filterWithKey $ \k _ -> k >= zero && k < zero + off
    let vars = blockData' <&> Map.keys . Map.filter ((^. _3) `fOr` (^. _2))
        varTypes = collectVarTypes (Set.fromList . concat $ Map.elems vars) bodyNode
    types <- traverse getType varTypes
    placeholders <- Map.elems <$> traverse (traverse $ \v -> (v,) . LO.LocalReference (fromJust $ Map.lookup v types)  <$> L.freshName (fromString $ T.unpack v)) (Map.delete zero vars)
    stackDeclVars <- translateMany stackdeclItems newVars
    headerVars <- translateMany header stackDeclVars
    finalVars <- traverse (translateBlock zero (Map.toList headerVars) placeholders) . fmap swap . sortOn fst $ fmap swap blocks
    return . Map.fromList . concat $ zipWith' zip' (fmap snd <$> placeholders) (fmap snd <$> finalVars)
    where
      stmtItems = [item | item@(Annot BodyStmt {} _) <- items]
      stackdeclItems = [item | item@(Annot BodyStackDecl {} _) <- items]
      _ = [item | item@(Annot BodyDecl {} _) <- items]
      (header, body) = tillNext stmtItems -- header = the rest of the entry block of the procedure
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

translateBlock :: TranslAssumps a m => Int ->  OutVars -> [OutVars] -> ([Annot BodyItem a], Int) -> m OutVars
translateBlock zero entry placeholders (~(item:items), idx) = do
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
                        else placeholders !! (f - zero - 1)
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

instance TranslAssumps a m =>
         Translate m BodyItem a (Map Text LO.Operand -> m (Map Text LO.Operand)) where
  translate (Annot (BodyDecl decl) _) = translate decl -- TODO ?
  translate (Annot (BodyStackDecl stackDecl) _) =
    translate stackDecl -- TODO ?
  translate (Annot (BodyStmt stmt) _) = translate stmt

instance TranslAssumps a m =>
         Translate m Decl a (Map Text LO.Operand -> m (Map Text LO.Operand)) -- TODO: continue from here
                                                               where
  translate _ = return -- TODO: continue from here

{- |
Guarantees:
- Every IfStmt has two bodies consisting of trivial goto statements
- Every SwitchStmt's arm consists of a trivial goto statement
-}
instance TranslAssumps a m =>
         Translate m Unit a (m ()) where
  translate (Unit topLevels `Annot` _) = do
    traverse_ translate $ topStructs <> topProcs <> topDecls <> topSections
      where
        topStructs = [struct | struct@(Annot TopStruct{} _) <- topLevels ]
        topProcs = [struct | struct@(Annot TopProcedure{} _) <- topLevels ]
        topDecls = [struct | struct@(Annot TopDecl{} _) <- topLevels ]
        topSections = [struct | struct@(Annot TopSection{} _) <- topLevels ]

instance TranslAssumps a m =>
         Translate m Stmt a (Map Text LO.Operand -> m (Map Text LO.Operand)) where
  translate (stmt `Annot` _) vars = case stmt of
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
      | CallStmt rets _ expr actuals Nothing [] <- stmt -> do
        actuals' <- traverse (`translate` vars) actuals
        expr' <- translate expr vars
        ret <- L.call expr' (fmap (,[]) actuals')
        extract <- if null rets
          then return []
          else (L.extractValue ret . pure) `traverse` [0..(fromInteger . toInteger $ length rets - 1)]
        let nVars = Map.fromList $ zip (getName <$> rets) extract
        return $ nVars <> vars
      | otherwise -> undefined
    JumpStmt {} -> undefined
    ReturnStmt {}
      | ReturnStmt Nothing Nothing actuals <- stmt -> do
        retType <- makePacked <$> traverse getType actuals
        ret <-  translateManyExprs vars actuals >>= makeTuple retType
        L.ret ret $> vars
      | otherwise -> undefined
    LabelStmt name -> (L.emitBlockStart . fromString . T.unpack . getName) name $> vars
    ContStmt {} -> undefined
    GotoStmt {}
      | GotoStmt _ Nothing <- stmt ->do
            let Just lab = getTrivialGotoTarget stmt -- TODO: is this safe?
            L.br (L.mkName $ T.unpack lab)
            return vars
      | otherwise -> undefined
    CutToStmt {} -> undefined
instance TranslAssumps a m =>
         Translate m Actual a (Map Text LO.Operand -> m LO.Operand) where
  translate (Annot (Actual Nothing expr) _) vars = translate expr vars
  translate _ _ = undefined

undef :: LT.Type -> LO.Operand
undef = LO.ConstantOperand . LC.Undef


makeTuple :: MonadTranslator m => LT.Type -> [LO.Operand] -> m LO.Operand
makeTuple _ [] = return . LO.ConstantOperand $ LC.Struct Nothing True []
makeTuple type' args = go 0 args (undef type')
  where
    go _ [] to' = return to'
    go i (first':others) to' =
      L.insertValue to' first' [i] >>= go (i + 1) others

translateManyExprs _ [] = return []
translateManyExprs vars (expr:exprs) =
  liftA2 (:) (translate expr vars) (translateManyExprs vars exprs)


-- Source: https://www.cs.tufts.edu/~nr/c--/extern/man2.pdf (7.4)
instance TranslAssumps a m =>
         Translate m Expr a (Map Text LO.Operand -> m LO.Operand) where
  translate (expr `Annot` _) vars = case expr of
    LitExpr lit Nothing -> translate lit
    ParExpr expr -> translate expr vars
    LVExpr lvalue -> translate lvalue vars
    BinOpExpr o l r -> do
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
    ComExpr expr -> do
      expr' <- translate expr vars
      L.typeOf expr' >>= \case
        Right (LT.IntegerType bits) ->
          L.xor expr' . LO.ConstantOperand $ LC.Int bits (-1)
        _ -> error "Cannot create a binary complement to a non-int"
    NegExpr expr ->
      translate expr vars >>= L.icmp L.EQ (LC.bit 0)
    MemberExpr expr@(_ `Annot` eAnnot) (Name name `Annot` _) -> do
      sName <- getStructName eAnnot
      ~(Just (accessors, _)) <- uses structs $ Map.lookup sName
      let Just idx = name `List.lookup` accessors
      expr' <- translate expr vars
      L.gep expr' [LC.int32 0, LC.int32 . fromInteger $ toInteger idx]

instance TranslAssumps a m =>
         Translate m Lit a (m LO.Operand) where
  translate (lit `Annot` annot) = case lit of
    LitInt int -> do
      t <- getIntWidth <$> getType annot
      return . LO.ConstantOperand $ LC.Int t  $ toInteger int -- TODO: discuss this later
    LitFloat float -> return $ LC.single float
    LitChar char -> do
      t <- getIntWidth <$> getType annot
      return . LO.ConstantOperand $ LC.Int t . toInteger $ ord char

-- TODO: continue from here
instance TranslAssumps a m =>
         Translate m LValue a (Map Text LO.Operand -> m LO.Operand) where
  translate (Annot lValue annot) vars = do
    records' <- use records
    case lValue of
      LVName name
        | Just op <- name' `Map.lookup` vars
        <|> name' `Map.lookup` records' -> return op
        where name' = getName name
      LVName name -> do
        t <- getType annot
        return . LO.ConstantOperand . LC.GlobalReference t $ translateName name

        -- TODO: traverse all frames (accessing global registers; also, accessing non-variables); remove the error
      LVRef Nothing expr Nothing ->
        translate expr vars >>= (`L.load` 0)
      LVRef {} ->
        error "dereferences not yet implemented" -- TODO: implement lvref

-- TODO: continue from here
instance TranslAssumps a m =>
         Translate m StackDecl a (Map Text LO.Operand -> m (Map Text LO.Operand)) where
  translate (StackDecl datums `Annot` _) = go mempty datums
    where
      go cache [] vars
        | null cache = return vars
        | otherwise = error $ "unresolved stackdata: " <> show cache
      go cache (Annot (datum :: Datum a) _:datums') vars = case datum of
        DatumLabel name -> go (getName name : cache) datums' vars
        Datum _ t Nothing Nothing -> do
          t' <- getType t
          record <- L.alloca t' Nothing 0
          cache `forM_` \item ->
            records %= Map.insert item record
          return vars
        Datum {} -> undefined
        DatumAlign {} -> undefined
