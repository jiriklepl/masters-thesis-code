{-# LANGUAGE Safe #-}

module CMM.AST.Blockifier.FilterDropped where

import safe Control.Lens ((%=), use, uses)
import safe Data.Bifunctor (Bifunctor(first))
import safe Data.Data (Data(gmapM), Typeable)
import safe Data.Foldable (for_, traverse_)
import safe Data.Generics.Aliases (extM)
import safe qualified Data.Graph as Graph
import safe qualified Data.List as List
import safe qualified Data.Map as Map
import safe Data.Maybe (fromMaybe)
import safe qualified Data.Set as Set

import safe qualified CMM.AST as AST
import safe CMM.AST.Annot (Annot, Annotation(Annot))
import safe CMM.AST.BlockAnnot
  ( BlockAnnot(Unreachable)
  , HasBlockAnnot
  , getBlockAnnot
  , getBlockMembership
  , setBlockAnnot
  )
import safe CMM.AST.Blockifier.Error
  ( BlockifierError(AlternativePath, ReDropping)
  )
import safe CMM.AST.Blockifier.State (Blockifier)
import safe qualified CMM.AST.Blockifier.State as State
import safe CMM.AST.GetName (GetName(getName))
import safe CMM.Data.Function (fOr)
import safe CMM.Parser.ASTError (registerASTError)
import safe CMM.Parser.GetPos (GetPos)

filterDropped ::
     (Data (n b), Data b, Typeable n, HasBlockAnnot b, GetPos b)
  => Annot n b
  -> Blockifier (Annot n b)
filterDropped n@(_ `Annot` (a :: annot)) = do
  drops <- use State.drops
  let dropFrom = Map.keysSet drops
  flow <- use State.currentFlow
  let altFlow =
        snd $
        List.partition
          ((`Set.member` dropFrom) . fst `fOr` (`Set.member` dropFrom) . snd)
          flow
  blocks <- use State.blocksCache
  zero <- uses State.allBlocks Map.size
  let bounds = (zero, zero + Map.size blocks - 1)
      graph = Graph.buildG bounds flow -- one block is guaranteed (procedure)
      reachable = Set.fromList $ Graph.reachable graph zero
      altGraph = Graph.buildG bounds altFlow
      newDrops =
        first (\v -> List.delete v $ Graph.reachable graph v) <$>
        Map.toList drops
      altReached = List.delete zero $ Graph.reachable altGraph zero
  newDrops `for_` \(targets, what) ->
    targets `for_` \target ->
      uses State.drops (Map.lookup target) >>= \case
        Just set''
          | null shared ->
            State.drops %= Map.insert target (set'' `Set.union` what)
          | otherwise ->
            shared `for_` \item -> registerASTError a $ ReDropping item
          where shared = set'' `Set.intersection` what
        Nothing -> State.drops %= Map.insert target what
  drops' <- use State.drops
  altReached `for_` \case
    alt
      | Just set'' <- alt `Map.lookup` drops' ->
        traverse_ (registerASTError a . AlternativePath) set''
    _ -> return ()
  let itemCase item@((_ :: AST.BodyItem annot) `Annot` annot) =
        gmapM go $ setBlockAnnot blockAnnot <$> item
        where
          blockAnnot =
            if isReachable
              then getBlockAnnot annot
              else Unreachable
          isReachable =
            (`Set.member` reachable) . fromMaybe (-1) $ getBlockMembership annot
      stmtCase (stmt `Annot` (annot :: annot)) =
        case stmt of
          AST.DroppedStmt {} -> empty
          AST.CallStmt _ _ expr act _ _
            | [AST.Actual _ expr' `Annot` _] <- act
            , AST.LVExpr lValue `Annot` _ <- expr
            , AST.LVExpr lValue' `Annot` _ <- expr'
            , AST.LVName name `Annot` _ <- lValue
            , AST.LVName name' `Annot` _ <- lValue'
            , Just idx <- getBlockMembership annot
            , Just dropped <- idx `Map.lookup` drops'
            , getName name == "drop"
            , getName name' `Set.member` dropped -> empty
          _ -> return $ stmt `Annot` annot
        where
          empty = return $ AST.EmptyStmt {} `Annot` annot
      go :: Data d => d -> Blockifier d
      go = gmapM go `extM` stmtCase `extM` itemCase
  go n
