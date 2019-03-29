{-# LANGUAGE TopLevelKindSignatures, PolyKinds #-}

module TopLevelKindSigConstraint where

import Data.Kind (Type)

-- See also: T16263
type Q :: Eq a => Type
data Q
