{-# LANGUAGE TopLevelKindSignatures, ScopedTypeVariables,
             TypeFamilies, TypeApplications,
             PolyKinds, ConstraintKinds #-}

module ScopedKindVariables where

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy)

type T :: forall k. k -> Type
data T a = MkT (Proxy (a :: k))

type S :: forall k. k -> Type
type S = T @k

type C :: forall k. k -> Constraint
class C a where
  getC :: T (a :: k)

{- Type family equations do not run under bindTyClTyVars,
   and thus have no access to scoped type variables.
--------------------------
type F :: forall k. k -> k
type family F a where
  F (Maybe a) = F @k a
  F x = x
-}
