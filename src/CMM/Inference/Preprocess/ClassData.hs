{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Inference.Preprocess.ClassData where

import safe Control.Lens (makeLenses)

import safe Data.Data (Data)
import safe Data.Set (Set)
import safe Data.Text (Text)

import safe CMM.Inference.Preprocess.Elaboration (Elaboration)

-- | Contains the type hole of the given class and the list of names of its methods
data ClassData =
  ClassData
    { _classHole :: Elaboration
    , _methodDecls :: Set Text
    }
  deriving (Data)

-- | Creates a `ClassData` object with the given parameters
initClassData :: Elaboration -> Set Text -> ClassData
initClassData hole decls = ClassData {_classHole = hole, _methodDecls = decls}

makeLenses ''ClassData
