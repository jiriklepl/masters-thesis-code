{-# LANGUAGE Safe #-}

module CMM.FlowAnalysis
  ( analyzeFlow
  ) where

import safe Control.Applicative ((<$), (<$>), (<*>))
import safe Control.Lens.Getter ((^.), use, uses)
import safe Control.Lens.Setter ((.=))
import safe Control.Lens.Tuple (_3)
import safe Control.Monad (fmap, return, unless)
import safe Data.Bool (Bool(False), (&&), (||), not)
import safe Data.Eq ((/=))
import safe Data.Foldable (null, or)
import safe Data.Function (($), (.), flip)
import safe qualified Data.Graph as Graph
import safe Data.Int (Int)
import safe Data.List (elemIndex, filter, sortOn, zip, zipWith)
import safe qualified Data.Map as Map
import safe Data.Monoid ((<>), mempty)
import safe qualified Data.Set as Set
import safe Data.Traversable (traverse)
import safe Data.Tuple (fst, swap)

import safe CMM.AST (Procedure)
import safe CMM.AST.Annot (Annot, Annotation(Annot))
import CMM.AST.Blockifier.Error
  ( BlockifierError(UninitializedRegisters, UnreachableContinuations,
                UnreachableLabels)
  )
import safe CMM.AST.Blockifier.State
  ( Blockifier
  , blockData
  , blocksTable
  , continuations
  , controlFlow
  , labels
  , registers
  )
import safe CMM.Data.Num ((-))
import safe CMM.Parser.ASTError (registerASTError, registerASTWarning)
import safe CMM.Parser.HasPos (HasPos)
import safe CMM.Pretty ()
import safe CMM.Utils (doWhile, hasPrefix)

analyzeFlow :: HasPos a => Annot Procedure a -> Blockifier ()
analyzeFlow procedure@(Annot _ _)
  -- TODO: implement `cut to` statements
 = do
  flow <- use controlFlow
  blocks <- use blocksTable
  let graph = Graph.buildG (0, Map.size blocks - 1) flow -- one block is guaranteed (procedure)
      blockNames = Map.fromList $ swap <$> Map.toList blocks
      reachable = Set.fromList $ Graph.reachable graph 0
      removeReachable = (`Map.withoutKeys` Set.map (blockNames Map.!) reachable)
      makeMessageFromVisible = filter (not . hasPrefix) . Map.keys
  labelsWarning <- makeMessageFromVisible <$> uses labels removeReachable
  continuationsWarning <-
    makeMessageFromVisible <$> uses continuations removeReachable
  unless (null labelsWarning && null continuationsWarning) $ do
    registerASTWarning procedure $ UnreachableLabels labelsWarning
    registerASTWarning procedure $ UnreachableContinuations continuationsWarning
  preCleanData <-
    uses registers (fmap . flip Map.restrictKeys . Map.keysSet) <*>
    uses blockData (`Map.restrictKeys` reachable) -- we filter out variables that are not local variables and whole blocks that are not reachable
  let allVars =
        (False, False, False) <$
        Map.foldlWithKey (\vars _ block -> block <> vars) mempty preCleanData
      cleanData = (<> allVars) <$> preCleanData -- we make sure that each block has a record for each variable
      order =
        flip elemIndex . filter (`Set.member` reachable) $ Graph.topSort graph
      cleanFlow =
        sortOn (order . fst) $ filter ((`Set.member` reachable) . fst) flow -- we filter out unreachable flow
  blockData .= cleanData
  controlFlow .= cleanFlow
  doWhile $ or <$> traverse updateFlowPair cleanFlow
  uninitialized <- uses blockData $ Map.keys . Map.filter (^. _3) . (Map.! 0)
  unless (null uninitialized) $
    registerASTError procedure $ UninitializedRegisters uninitialized

-- TODO: "unused after write" warning
updateFlowPair :: (Int, Int) -> Blockifier Bool
updateFlowPair (f, t) = do
  blocks <- use blockData
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
  blockData .= newBlocks
  return . or $
    zipWith
      (\a b -> a ^. _3 /= b ^. _3)
      (Map.elems newBlock)
      (Map.elems fromVars)
  where
    updateFlowPairVar ((name, (r, w, l)), (_, (_, _, l'))) =
      (name, (r, w, l || (not w && l')))
