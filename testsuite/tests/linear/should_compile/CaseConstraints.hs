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

-- A function of a linear implicit argument, uses it twice (like return (?x, ?x)). Throws a type error.
module Ex2 where

import GHC.Classes
import GHC.Exts hiding (Ptr)
import GHC.Prim
import GHC.Types
import GHC.TypeLits

import Prelude hiding ((>>=), return)
import Unsafe.Coerce

bar :: (?foo :: Int) => Bool -> Int
bar x = if x then ?foo else ?foo + ?foo
