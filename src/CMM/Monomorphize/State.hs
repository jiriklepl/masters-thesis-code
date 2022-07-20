{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}

module CMM.Monomorphize.State
  ( module CMM.Monomorphize.State
  , module CMM.Monomorphize.State.Impl
  ) where

import safe Control.Lens (ASetter', Lens', (%=), (+=), (.=), (<>=), use, uses)
import safe Control.Monad (unless, when)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Set (Set)
import safe qualified Data.Set as Set

import safe CMM.AST.Annot (Annot)
import safe CMM.AST.Wrap (ASTWrapper)
import safe CMM.Inference.Fact (Scheme)
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeVar (TypeVar)
import safe CMM.Monomorphize.Schematized (Schematized)
import safe CMM.Parser.GetPos (GetPos(getPos))

import safe CMM.Monomorphize.State.Impl
  ( MonomorphizeState
  , Monomorphizer
  , PolyData(PolyData, getPolyData)
  , PolyGenerate(PolyGenerate, getPolyGenerate)
  , PolyMemory(PolyMemory, getPolyMemory)
  , PolyMethods(PolyMethods)
  , PolyMethods(getPolyMethods)
  , initMonomorphizeState
  , maxPolyWaves
  , polyData
  , polyGenerate
  , polyMemory
  , polyMethods
  , polySchemes
  , polyStorage
  , polyWaves
  )

-- | resets the list of items to monomorphize
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

memorizeImpl ::
     Lens' (MonomorphizeState a) PolyMemory
  -> TypeVar
  -> Type
  -> Monomorphizer a ()
memorizeImpl toWhere scheme inst = addImpl PolyMemory scheme inst toWhere

memorize :: TypeVar -> Type -> Monomorphizer a ()
memorize = memorizeImpl polyMemory

-- | increments the number of currently done monomorphization waves and returns the number of the next wave
incWaves :: Monomorphizer a Int
incWaves = do
  polyWaves += 1
  use polyWaves

-- | returns the current number of monomorphization waves
getWaves :: Monomorphizer a Int
getWaves = use polyWaves

-- | returns the maximum allowed number of monomorphization waves
getMaxWaves :: Monomorphizer a Int
getMaxWaves = use maxPolyWaves

isMemorizedImpl ::
     Lens' (MonomorphizeState a) PolyMemory
  -> TypeVar
  -> Type
  -> Monomorphizer a Bool
isMemorizedImpl inWhere scheme inst =
  uses inWhere $ maybe False (Set.member inst) . Map.lookup scheme .
  getPolyMemory

-- | returns `True` iff the given scheme is memorized with the given type
isMemorized :: TypeVar -> Type -> Monomorphizer a Bool
isMemorized = isMemorizedImpl polyMemory

tryMemorizeImpl ::
     Lens' (MonomorphizeState a) PolyMemory
  -> TypeVar
  -> Type
  -> Monomorphizer a Bool
tryMemorizeImpl toWhere scheme inst = do
  memorized <- isMemorizedImpl toWhere scheme inst
  unless memorized $ memorizeImpl toWhere scheme inst
  return $ not memorized

-- | returns `True` iff memorizing the given scheme with the given type was successful
tryMemorize :: TypeVar -> Type -> Monomorphizer a Bool
tryMemorize = tryMemorizeImpl polyMemory

-- | returns `True` iff storing the given scheme with the given type was successful (like memorizing, but from within the function)
tryStore :: TypeVar -> Type -> Monomorphizer a Bool
tryStore = tryMemorizeImpl polyStorage

-- | attempts to memorize a given scheme representing the given wrapped object
--   represented by the given instance with the given type, if successful, adds it to the list of items to be generated
addGenerate ::
     GetPos a
  => Annot ASTWrapper a
  -> TypeVar
  -> TypeVar
  -> Type
  -> Monomorphizer a ()
addGenerate n scheme inst instType = do
  success <- tryMemorize scheme instType
  when success $ polyGenerate %= PolyGenerate .
    Map.insertWith mappend scheme [(inst, getPos <$> n)] .
    getPolyGenerate

-- | adds the given method scheme or struct scheme represented by the given type variable and with the given `Schematized` object
--   to the database of schemes
addPolyScheme :: TypeVar -> Scheme Type -> Schematized a -> Monomorphizer a ()
addPolyScheme tVar scheme schematized =
  polySchemes %= Map.insert tVar (scheme, schematized)

-- | adds the instance to the list of instances of the method given by its scheme
addMethod :: TypeVar -> TypeVar -> Monomorphizer a ()
addMethod scheme inst = addImpl PolyMethods scheme inst polyMethods

-- | adds an instance of the given field in the given struct
addField :: TypeVar -> TypeVar -> TypeVar -> Monomorphizer a ()
addField struct field inst =
  polyData <>= PolyData (Map.singleton field (Map.singleton inst struct))
