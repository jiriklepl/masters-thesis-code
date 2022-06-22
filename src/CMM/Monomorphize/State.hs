{-# LANGUAGE Safe #-}

module CMM.Monomorphize.State (module CMM.Monomorphize.State, module CMM.Monomorphize.State.Impl) where

import safe Data.Map ( Map )
import safe Data.Set ( Set )
import safe Data.Semigroup ( Semigroup )
import qualified Data.Map as Map
import safe Data.Monoid ( Monoid(mempty) )
import safe Data.Function ( ($), (.) )
import safe Control.Lens.Setter ( (.=), (%~), (<>=), ASetter' )
import safe Control.Lens.Tuple ( Field1(_1), Field2(_2) )
import safe Control.Lens.Getter ( uses )
import safe Data.Functor ( Functor(fmap), (<$>) )
import safe Data.Bool ( Bool(False), not )
import qualified Data.Set as Set
import safe Data.Maybe ( maybe )
import safe Control.Monad ( Monad(return), unless, when )

import safe CMM.Inference.TypeVar ( TypeVar )
import safe CMM.Inference.TypeHandle ( TypeHandle )
import safe CMM.Inference.Subst ( Apply(apply) )

import CMM.Monomorphize.State.Impl (Monomorphizer, MonomorphizeState, PolyData (getPolyData, PolyData), polyData, polyGenerate, PolyGenerate (PolyGenerate, getPolyGenerate), polyMemory, PolyMethods (PolyMethods), polyMethods, polySchemes, PolyMethods (getPolyMethods), initMonomorphizeState)

renewPolyData :: Map TypeVar TypeVar -> Monomorphizer a ()
renewPolyData (unifs :: Map TypeVar TypeVar) = do
  fromData <- uses polyData $ fmap (_2 %~ Map.toList) . Map.toList . getPolyData
  let toData = Map.fromList $ (_2 %~ Map.fromList . fmap (apply unifs)) . (_1 %~ apply unifs) <$> fromData
  polyData .= PolyData toData

unPolyGenerate :: Monomorphizer a ()
unPolyGenerate = polyGenerate .= mempty

addImpl :: (Semigroup a1) =>
  (Map k (Set a2) -> a1) -> k -> a2 -> ASetter' (MonomorphizeState a) a1 -> Monomorphizer a ()
addImpl which scheme inst = (<>= which (Map.singleton scheme $ Set.singleton inst))

memorize :: TypeHandle -> TypeHandle -> Monomorphizer a ()
memorize scheme inst = addImpl PolyGenerate scheme inst polyMemory

isMemorized ::  TypeHandle -> TypeHandle -> Monomorphizer a Bool
isMemorized scheme inst =
  uses polyMemory $ maybe False (Set.member inst) . Map.lookup scheme . getPolyGenerate

tryMemorize :: TypeHandle -> TypeHandle -> Monomorphizer a Bool
tryMemorize scheme inst = do
  memorized <- isMemorized scheme inst
  unless memorized $ memorize scheme inst
  return $ not memorized

addGenerate :: TypeHandle -> TypeHandle -> Monomorphizer a ()
addGenerate scheme inst = do
  success <- tryMemorize scheme inst
  when success $ addImpl PolyGenerate scheme inst polyGenerate


addMethod :: TypeHandle -> TypeHandle -> Monomorphizer a ()
addMethod scheme inst = addImpl PolyMethods scheme inst polyMethods

addData :: TypeVar
  -> TypeVar
  -> TypeVar
  -> Monomorphizer a ()
addData struct field inst = polyData <>= PolyData (Map.singleton  field (Map.singleton inst struct))