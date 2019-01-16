{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnsaturatedTypeFamilies #-}
module Test where

import GHC.TypeLits
import GHC.Types
import Data.Proxy

import Type.Reflection

type IdSyn a = a

type family Id a where
  Id a = a

data T (f :: * ->{m} *) = T (f Int)

deriving instance (Show (f Int)) => Show (T (f :: * ->{m} *))

class Functor' (f :: * ->{m} *) where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' Maybe where
  fmap' = fmap

instance (f ~ Id) => Functor' f where
  fmap' = id


ambiguous :: forall m a b (f :: * ->{m} *). (a -> b) -> f a -> f b
ambiguous = undefined

-- TODO: With the synonym, an assertion fails (in TcUnify)
-- testU1 :: T IdSyn
-- testU1 = T 10

testU2 :: T Id
testU2 = T 10

testM :: T Maybe
testM = T (Just 10)

--------------------------------------------------------------------------------------

type Test1 = Map ((+) 10) '[1,2,3]
type Test2 = Map 'Just '[1,2,3]

----------------------------------------------------------------------------------
----

type family Compute (f :: a ->{m} b) :: Matchability where
  Compute (f :: _ ->{m} _) = m

type Test3 = Compute Maybe
type Test4 = Compute Id

---- works perfectly..

------------------------------------------------------------------------------------

type family IdF a where
  IdF a = a

type family IdF2 :: * ~> * where
  IdF2 = IdF

type family IdF3 :: * ~> * where
  IdF3 = Id

----------------------------------------------------------------------------------

---- good!
--
-- Could not deduce a ~ b
-- from the context f a ~ g b
--
-- decomp1 :: ((f :: * ~> *) a ~ g b) => a -> b
-- decomp1 = id

------------------------------------------------------------------------------------
---- Fine!
---- TODO: can't call it from GHCi without arguments!
---- occurs @Id is fine.
occurs :: forall (f :: * ~> *) a. a ~ f a => ()
occurs = ()

idF :: forall (f :: * ~> *) a. (a ~ f a) => a -> f a
idF = id

----------------------------------------------------------------------------------


---- perfect (other than the explicit matchability in the pattern)
type family Flip (f :: j ->{m} k ->{n} l) (a :: k) (b :: j)  :: l where
  Flip f a b = f b a

type family (<<<) (f :: b ->{m} c) (g :: a ->{n} b) (x :: a) :: c where
  (f <<< g) x = f (g x)
infixr 9 <<<

----------------------------------------------------------------------------------
foo :: (f :: * ~> *) ~ Id => f Int
foo = (10 :: Int)

-- bar :: (f ~ g, g ~ Id) => f Int
-- bar = (10 :: Int)

type family All (ts :: [Type]) (c :: Type -> Constraint) :: Constraint where
  All '[] _ = ()
  All (x ': xs) c = (c x, All xs c)

type family Map (f :: a ->{m} b) (as :: [a]) :: [b] where
  Map _ '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

data HList xs where
  Nil   :: HList '[]
  (:>)  :: a -> HList as -> HList (a ': as)

infixr 5 :>

deriving instance All xs Show => Show (HList xs)

class Db a where
  type family DbType a
  toDb :: a -> DbType a

data DbString = DbString String
  deriving Show

instance Db String where
  type instance DbType String = DbString
  toDb = undefined

data DbInt = DbInt Int
  deriving Show

instance Db Int where
  type instance DbType Int = DbInt
  toDb = DbInt

--------------------------------------------------------------------------------
hmap :: forall m c (f :: * ->{m} *) a ts. All ts c => (forall a. c a => a -> f a) -> HList ts -> HList (Map f ts)
hmap _ Nil          = Nil
hmap f (x :> xs)    = f x :> hmap @_ @c @f f xs

test :: All ts Db => HList ts -> HList (Map DbType ts)
test = hmap @_ @Db @DbType toDb


type family UnApp (f :: *) :: * where
  UnApp (f a) = a
  --UnApp ((f :: * ~> *) a) = a

type family NotPoly (f :: a ->{m} b) :: * ->{m} * where
  NotPoly f = Maybe
  NotPoly f = Id

type family Nest0 :: a ~> b

-- type family Id (a :: Type) :: Type where
--   Id a = a

type family Open a :: Type ~> Type

type family Const a b where
  Const a b = a


type family Nest1 a where
  Nest1 x = Nest2 x

type family Nest2 a b where
  Nest2 a b = Nest3 a b

type family Nest3 a b c where
  Nest3 a b c = c

-- :kind! Nest1 Int Bool Char
-- = Char

blah :: Nest1 Int Bool Char
blah = 'c'

bar :: (f :: * ~> *) a -> f a
bar = id

-- doesn't compile! (and it shouldnt't)
-- goo :: forall (g :: * ~> *) a. g a -> g a
-- goo = bar

goo :: forall (g :: * ~> *) a. g a -> g a
goo = bar @g @a

type family Foldl (f :: b ->{m} a ->{m} b) (z :: b) (xs :: [a]) :: b where
  Foldl f z '[] = z
  Foldl f z (x ': xs) = Foldl f (f z x) xs
