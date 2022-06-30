{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}


module CMM.AST.Flattener.State where

import Control.Lens
    ( use, uses, (%=), (+=), (.=), makeFieldsNoPrefix )
import Control.Monad.State ( State )
import Data.Map (Map)
import Data.Text (Text)
import CMM.AST ( Name(Name) )
import qualified Data.Text as T
import CMM.Utils ( addPrefix )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Foldable ( for_ )



data FlattenerState
  = FlattenerState
    { _branchIdHandles :: Int
    , _contractorMap :: Map Text Text
    , _contractIdHandles :: Int
    }
    deriving (Show)


initFlattenerState :: FlattenerState
initFlattenerState =
  FlattenerState
    { _branchIdHandles = 0
    , _contractIdHandles = 0
    , _contractorMap = mempty
    }

type Flattener = State FlattenerState

makeFieldsNoPrefix ''FlattenerState

-- | Generates a fresh integer
freshBranch :: Flattener Int
freshBranch = do
  branchIdHandles += 1
  use branchIdHandles

freshBranchNum :: Flattener String
freshBranchNum = fmap show freshBranch

-- | Generates a fresh integer
freshContract :: Flattener Int
freshContract = do
  contractIdHandles += 1
  use contractIdHandles

registerContractors :: [Text] -> Text -> Flattener ()
registerContractors contractors contract = do
  (contract:contractors) `for_` \contractor ->
    contractorMap %= Map.insert contractor contract

getContracts :: Flattener (Set Text)
getContracts = do
  uses contractorMap (Set.fromList . Map.elems)

freshContractName :: Flattener (Name a)
freshContractName = do
  num <- freshContract
  return . helperName $ "contract_" ++ show num

clearFlattener :: Flattener ()
clearFlattener = do
  branchIdHandles .= 0
  contractIdHandles .= 0
  contractorMap .= mempty

-- | Creates a `Name` from a `String` with `flattenerPrefix`
helperName :: String -> Name a
helperName = Name . addPrefix flattenerPrefix . T.pack

flattenerPrefix :: Text
flattenerPrefix = "F"


