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

lin :: a %1-> a
lin x = x

nonlin :: a -> a
nonlin x = x

bad :: (?foo :: Int) =>. Bool -> Int
bad x = if x then lin ?foo else nonlin ?foo
--     • Could not deduce: ?foo::Int
--         arising from a use of implicit parameter ‘?foo’
--       from the context: ?foo::Int
--         bound by the type signature for:
--                    bar :: (?foo::Int) => 'One Bool -> Int
--         at testsuite/tests/linear/should_compile/CaseConstraints.hs:33:1-36
--     • In the first argument of ‘nonlin’, namely ‘?foo’
--       In the expression: nonlin ?foo
--       In the expression: if x then lin ?foo else nonlin ?foo
--    |
-- 34 | bar x = if x then lin ?foo else nonlin ?foo
--    |                                        ^^^^

good :: (?foo :: Int) =>. Bool -> Int
good x = if x then lin ?foo else lin ?foo
-- typechecks
