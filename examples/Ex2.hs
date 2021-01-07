{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RebindableSyntax #-}

-- A function of a linear implicit argument, uses it twice (like return (?x, ?x)). Throws a type error.
module Ex2 where

import GHC.Classes
import GHC.Exts hiding (Ptr)
import GHC.Prim
import GHC.Types
import GHC.TypeLits

import Prelude hiding ((>>=), return)
import Unsafe.Coerce

foo :: (?kakimaki :: Int) => Int
foo = (10 :: Int) + ?kakimaki -- + let ?kakimaki = (20 :: Int) in ?kakimaki

asd :: Bool ->. Int
asd x = case x of
  True -> 10
  False -> 10
-- asd True = 10
-- asd False = 10
