{-# LANGUAGE TopLevelKindSignatures, TypeFamilyDependencies,
             MultiParamTypeClasses, GADTs, RankNTypes, FlexibleContexts,
             DataKinds, PolyKinds, ConstraintKinds #-}

module TopLevelKindSignatures where

import Data.Proxy (Proxy)
import GHC.TypeLits (Nat)
import Data.Kind (Type, Constraint)

type MonoTagged :: Type -> Type -> Type
data MonoTagged t x = MonoTagged x

type Id :: forall k. k -> k
type family Id x where
  Id x = x

type InjectiveId :: forall k. k -> k
type family InjectiveId x = r | r -> x where
  InjectiveId x = x

type C :: (k -> Type) -> k -> Constraint
class C a b where
  f :: a b

type TypeRep :: forall k. k -> Type
data TypeRep a where
  TyInt   :: TypeRep Int
  TyMaybe :: TypeRep Maybe
  TyApp   :: TypeRep a -> TypeRep b -> TypeRep (a b)

-- type D :: j -> Constraint -- #16571
type D :: Type -> Constraint
type D = C TypeRep

-- type DF :: j -> Constraint -- #16571
type DF :: Type -> Constraint
type family DF where
  DF = C TypeRep

type family F a where { F Bool = Nat; F Nat = Type }
type family G a where { G Type = Type -> Type; G () = Nat }

type X :: forall k1 k2. (F k1 ~ G k2) => k1 -> k2 -> Type
data X a b where
  MkX :: X 'True '()         -- accepted
  -- MkX :: X 'True 'False   -- rejected, see Note [QualTy in kinds]
  -- FIXME (int-index): not rejected at the moment

-- Test inferred type variables.
-- T :: forall {k} (a :: k). Proxy a -> Type
type T :: Proxy a -> Type
data T x = MkT

type Q :: forall k -> k -> Type
data Q j (a :: j)

type W :: forall (a :: forall k. k -> Type) -> a Int -> a Maybe -> Type
data W x (y :: x Int) (z :: x Maybe)
