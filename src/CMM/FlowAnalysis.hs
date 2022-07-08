{-# LANGUAGE Safe #-}

module CMM.FlowAnalysis
  ( analyzeFlow
  ) where

import safe Control.Lens ( (^.), use, uses, (.=), _3 )
import safe Control.Monad (unless)
import safe qualified Data.Graph as Graph
import safe Data.List (elemIndex, sortOn )
import safe qualified Data.Map as Map
import safe qualified Data.Set as Set
import safe Data.Tuple (swap)

import safe CMM.AST (Procedure)
import safe CMM.AST.Annot (Annot, Annotation(Annot))
import CMM.AST.Blockifier.Error
  ( BlockifierError(UninitializedRegisters, UnreachableContinuations,
                UnreachableLabels)
  )
import safe qualified CMM.AST.Blockifier.State as State
import safe CMM.AST.Blockifier.State (Blockifier)
import safe CMM.Parser.ASTError (registerASTError, registerASTWarning)
import safe CMM.Parser.GetPos (GetPos)
import safe CMM.Pretty ()
import safe CMM.Utils (doWhile, hasPrefix)
import safe Data.Maybe ( fromJust )
import safe Data.Functor ( (<&>) )

analyzeFlow :: GetPos a => Annot Procedure a -> Blockifier ()
analyzeFlow procedure@(Annot _ _)
 = do
  flow <- use State.currentFlow
  blocks <- use State.blocksCache
  zero <- uses State.allBlocks Map.size
  let graph = Graph.buildG (zero, zero + Map.size blocks - 1) flow -- one block is guaranteed (procedure)
      blockNames = Map.fromList $ swap <$> Map.toList blocks
      reachable = Set.fromList $ Graph.reachable graph zero
      removeReachable = (`Map.withoutKeys` Set.map (fromJust . (`Map.lookup` blockNames)) reachable)
      makeMessageFromVisible = filter (not . hasPrefix) . Map.keys
  labelsWarning <- makeMessageFromVisible <$> uses State.labels removeReachable
  continuationsWarning <-
    makeMessageFromVisible <$> uses State.continuations removeReachable
  unless (null labelsWarning && null continuationsWarning) $ do
    registerASTWarning procedure $ UnreachableLabels labelsWarning
    registerASTWarning procedure $ UnreachableContinuations continuationsWarning
  preCleanData <-
    uses State.registers (fmap . flip Map.restrictKeys . Map.keysSet) <*>
    uses State.cacheData (`Map.restrictKeys` reachable) -- we filter out variables that are not local variables and whole blocks that are not reachable
  let allVars =
        (False, False, False) <$
        Map.foldlWithKey (\vars _ block -> block <> vars) mempty preCleanData
      cleanData = (<> allVars) <$> preCleanData -- we make sure that each block has a record for each variable
      order =
        flip elemIndex . filter (`Set.member` reachable) $ Graph.topSort graph
      cleanFlow =
        sortOn (order . fst) $ filter ((`Set.member` reachable) . fst) flow -- we filter out unreachable flow
  State.cacheData .= cleanData
  State.currentFlow .= cleanFlow
  doWhile $ or <$> traverse updateFlowPair cleanFlow
  uninitialized <- uses State.cacheData (Map.lookup zero) <&> \case
    Nothing -> []
    Just blockData -> Map.keys $ Map.filter (^. _3) blockData
  unless (null uninitialized) $
    registerASTError procedure $ UninitializedRegisters uninitialized

updateFlowPair :: (Int, Int) -> Blockifier Bool
updateFlowPair (f, t) = do
  blocks <- use State.cacheData
  let toVars = fromJust $ t `Map.lookup` blocks
      fromVars = fromJust $ f `Map.lookup` blocks
      newBlocks =
        Map.insertWithKey
          (\_ new old ->
             Map.fromAscList $
             updateFlowPairVar <$> zip (Map.toAscList old) (Map.toAscList new))
          f
          toVars
          blocks
      newBlock = fromJust $ f `Map.lookup` newBlocks
  State.cacheData .= newBlocks
  return . or $
    zipWith
      (\a b -> a ^. _3 /= b ^. _3)
      (Map.elems newBlock)
      (Map.elems fromVars)
  where
    updateFlowPairVar ((name, (r, w, l)), (_, (_, _, l'))) =
      (name, (r, w, l || (not w && l')))
