{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Inference.Preprocess.ClassData where

import safe Control.Lens.TH (makeLenses)

import safe Data.Set (Set)
import safe Data.Text (Text)

import safe CMM.Inference.Preprocess.TypeHole (TypeHole)

data ClassData =
  ClassData
    { _classHole :: TypeHole
    , _methodDecls :: Set Text
    }

initClassData :: TypeHole -> Set Text -> ClassData
initClassData handle decls =
  ClassData {_classHole = handle, _methodDecls = decls}

makeLenses ''ClassData
