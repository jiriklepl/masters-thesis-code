{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}

module CMM.Monomorphize.State
  ( module CMM.Monomorphize.State
  , module CMM.Monomorphize.State.Impl
  ) where

import safe Control.Lens.Getter (uses, use)
import safe Control.Lens.Setter (ASetter', (%=), (%~), (.=), (<>=), (+=))
import safe Control.Lens.Tuple (Field1(_1), Field2(_2))
import safe Control.Monad (Monad(return), unless, when)
import safe Data.Bool (Bool(False), not)
import safe Data.Function (($), (.))
import safe Data.Functor (Functor(fmap), (<$>))
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe (maybe)
import safe Data.Monoid (Monoid(mempty, mappend))
import safe Data.Semigroup (Semigroup)
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Control.Lens.Lens (Lens')
import safe Data.Int ( Int )

import safe CMM.Inference.Fact (Scheme)
import safe CMM.Inference.Subst (Apply(apply))
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeVar (TypeVar)
import safe CMM.Monomorphize.Schematized (Schematized)
import safe CMM.Parser.HasPos ( HasPos(getPos) )
import safe CMM.AST.Wrap ( ASTWrapper )
import safe CMM.AST.Annot ( Annot )

import safe CMM.Monomorphize.State.Impl
  ( MonomorphizeState
  , Monomorphizer
  , PolyData(PolyData, getPolyData)
  , PolyGenerate(PolyGenerate, getPolyGenerate)
  , PolyMethods(PolyMethods)
  , PolyMethods(getPolyMethods)
  , initMonomorphizeState
  , polyData
  , polyGenerate
  , polyMemory
  , polyMethods
  , polySchemes
  , polyStorage
  , polyWaves
  , maxPolyWaves, PolyMemory (PolyMemory, getPolyMemory)
  )

renewPolyData :: Map TypeVar TypeVar -> Monomorphizer a ()
renewPolyData (unifs :: Map TypeVar TypeVar) = do
  fromData <- uses polyData $ fmap (_2 %~ Map.toList) . Map.toList . getPolyData
  let toData =
        Map.fromList $ (_2 %~ Map.fromList . fmap (apply unifs)) .
        (_1 %~ apply unifs) <$>
        fromData
  polyData .= PolyData toData

unPolyGenerate :: Monomorphizer a ()
unPolyGenerate = polyGenerate .= mempty

addImpl ::
     (Semigroup a1)
  => (Map k (Set a2) -> a1)
  -> k
  -> a2
  -> ASetter' (MonomorphizeState a) a1
  -> Monomorphizer a ()
addImpl which scheme inst =
  (<>= which (Map.singleton scheme $ Set.singleton inst))

memorizeImpl :: Lens' (MonomorphizeState a) PolyMemory -> TypeVar -> Type -> Monomorphizer a ()
memorizeImpl toWhere scheme inst = addImpl PolyMemory scheme inst toWhere

memorize :: TypeVar -> Type -> Monomorphizer a ()
memorize = memorizeImpl polyMemory

store :: TypeVar -> Type -> Monomorphizer a ()
store = memorizeImpl polyStorage

incWaves :: Monomorphizer a Int
incWaves = do
  polyWaves += 1
  use polyWaves

getMaxWaves :: Monomorphizer a Int
getMaxWaves =
  use maxPolyWaves

isMemorizedImpl :: Lens' (MonomorphizeState a) PolyMemory -> TypeVar -> Type -> Monomorphizer a Bool
isMemorizedImpl inWhere scheme inst =
  uses inWhere $ maybe False (Set.member inst) . Map.lookup scheme .
  getPolyMemory

isMemorized :: TypeVar -> Type -> Monomorphizer a Bool
isMemorized = isMemorizedImpl polyMemory

isStored :: TypeVar -> Type -> Monomorphizer a Bool
isStored = isMemorizedImpl polyStorage

tryMemorizeImpl :: Lens' (MonomorphizeState a) PolyMemory -> TypeVar -> Type -> Monomorphizer a Bool
tryMemorizeImpl toWhere scheme inst = do
  memorized <- isMemorizedImpl toWhere scheme inst
  unless memorized $ memorizeImpl toWhere scheme inst
  return $ not memorized

tryMemorize :: TypeVar -> Type -> Monomorphizer a Bool
tryMemorize  = tryMemorizeImpl polyMemory

tryStore :: TypeVar -> Type -> Monomorphizer a Bool
tryStore = tryMemorizeImpl polyStorage

addGenerate :: HasPos a => Annot ASTWrapper a -> TypeVar -> TypeVar -> Type -> Monomorphizer a ()
addGenerate n scheme inst instType = do
  success <- tryMemorize scheme instType
  when success $ polyGenerate %= PolyGenerate . Map.insertWith mappend scheme [(inst, getPos <$> n)] . getPolyGenerate

addPolyScheme :: TypeVar -> Scheme Type -> Schematized a -> Monomorphizer a ()
addPolyScheme tVar scheme schematized =
  polySchemes %= Map.insert tVar (scheme, schematized)

addMethod :: TypeVar -> TypeVar -> Monomorphizer a ()
addMethod scheme inst = addImpl PolyMethods scheme inst polyMethods

addData :: TypeVar -> TypeVar -> TypeVar -> Monomorphizer a ()
addData struct field inst =
  polyData <>= PolyData (Map.singleton field (Map.singleton inst struct))
