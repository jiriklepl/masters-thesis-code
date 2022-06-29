{-# LANGUAGE Safe #-}

module CMM.FlowAnalysis
  ( analyzeFlow
  ) where

import safe Control.Lens.Getter ((^.), use, uses)
import safe Control.Lens.Setter ((.=))
import safe Control.Lens.Tuple (_3)
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
import safe CMM.Parser.HasPos (HasPos)
import safe CMM.Pretty ()
import safe CMM.Utils (doWhile, hasPrefix)

analyzeFlow :: HasPos a => Annot Procedure a -> Blockifier ()
analyzeFlow procedure@(Annot _ _)
  -- TODO: implement `cut to` statements
 = do
  flow <- use State.controlFlow
  blocks <- use State.blocksTable
  let graph = Graph.buildG (0, Map.size blocks - 1) flow -- one block is guaranteed (procedure)
      blockNames = Map.fromList $ swap <$> Map.toList blocks
      reachable = Set.fromList $ Graph.reachable graph 0
      removeReachable = (`Map.withoutKeys` Set.map (blockNames Map.!) reachable)
      makeMessageFromVisible = filter (not . hasPrefix) . Map.keys
  labelsWarning <- makeMessageFromVisible <$> uses State.labels removeReachable
  continuationsWarning <-
    makeMessageFromVisible <$> uses State.continuations removeReachable
  unless (null labelsWarning && null continuationsWarning) $ do
    registerASTWarning procedure $ UnreachableLabels labelsWarning
    registerASTWarning procedure $ UnreachableContinuations continuationsWarning
  preCleanData <-
    uses State.registers (fmap . flip Map.restrictKeys . Map.keysSet) <*>
    uses State.blockData (`Map.restrictKeys` reachable) -- we filter out variables that are not local variables and whole blocks that are not reachable
  let allVars =
        (False, False, False) <$
        Map.foldlWithKey (\vars _ block -> block <> vars) mempty preCleanData
      cleanData = (<> allVars) <$> preCleanData -- we make sure that each block has a record for each variable
      order =
        flip elemIndex . filter (`Set.member` reachable) $ Graph.topSort graph
      cleanFlow =
        sortOn (order . fst) $ filter ((`Set.member` reachable) . fst) flow -- we filter out unreachable flow
  State.blockData .= cleanData
  State.controlFlow .= cleanFlow
  doWhile $ or <$> traverse updateFlowPair cleanFlow
  uninitialized <- uses State.blockData $ Map.keys . Map.filter (^. _3) . (Map.! 0)
  unless (null uninitialized) $
    registerASTError procedure $ UninitializedRegisters uninitialized

-- TODO: "unused after write" warning
updateFlowPair :: (Int, Int) -> Blockifier Bool
updateFlowPair (f, t) = do
  blocks <- use State.blockData
  let toVars = blocks Map.! t
      fromVars = blocks Map.! f
      newBlocks =
        Map.insertWithKey
          (\_ new old ->
             Map.fromAscList $
             updateFlowPairVar <$> zip (Map.toAscList old) (Map.toAscList new))
          f
          toVars
          blocks
      newBlock = newBlocks Map.! f
  State.blockData .= newBlocks
  return . or $
    zipWith
      (\a b -> a ^. _3 /= b ^. _3)
      (Map.elems newBlock)
      (Map.elems fromVars)
  where
    updateFlowPairVar ((name, (r, w, l)), (_, (_, _, l'))) =
      (name, (r, w, l || (not w && l')))
