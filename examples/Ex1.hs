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
module Ex1 where

import GHC.Classes
import GHC.Exts hiding (Ptr)
import GHC.Prim
import GHC.Types
import GHC.TypeLits

import Prelude hiding ((>>=), return)
import Unsafe.Coerce

f :: Int -> Int -> Int
f _ _ = 0

-- good' :: Int ->. Int
-- good' x = f x

data T m where
  MkOne   :: T 'One
  MkOmega :: T 'Omega

-- lin :: Int ->. Int
-- lin x = x

-- -- bad :: (?x :: Int) =>. (Int, Int)
-- -- bad = (?x, ?x)
-- -- examples/Ex1.hs:1:1: error:
-- --     Couldn't match expected multiplicity ‘'One’ of variable ‘$dIP_a2cn’ with actual multiplicity ‘'Omega’
-- --   |
-- -- 1 | {-# LANGUAGE DataKinds #-}
-- --   | ^

-- -- bad2 :: (?x :: Int) =>. (Int, Int)
-- -- bad2 = (good, good)
-- -- examples/Ex1.hs:1:1: error:
-- --     Couldn't match expected multiplicity ‘'One’ of variable ‘$dIP_a2cq’ with actual multiplicity ‘'Omega’
-- --   |
-- -- 1 | {-# LANGUAGE DataKinds #-}
-- --   | ^


-- -- shouldn't type check!
-- bad3 :: (?x :: Int) =>. Int
-- bad3 = 10

-- goodShow :: Show a =>. a -> String
-- goodShow x = show x

-- -- badShow :: Show a =>. a -> String
-- -- badShow x = show x ++ show x

-- foo :: (?x :: Int) =>. Bool -> Int
-- foo True = ?x
-- foo False= ?x

-- bar :: (?x :: Int) =>. Bool -> Int
-- bar b = case b of
--   True -> ?x
--   False -> ?x

foldBool :: a -> a -> Bool -> a
foldBool x _ True = x
foldBool _ x False = x

data A
data B

-- f :: A -> B
-- f = undefined

-- g :: (?x :: A) =>. B
-- g = f ?x


-- doesn't work... only with 'if'
-- linearCase :: Bool ->. Int
-- linearCase b = case b of
--   True -> 10
--   False -> 20

error' :: a -> b ->. c
error' = error'

linear1 :: Int ->. Bool -> Int
linear1 x True = x
linear1 x False = error' "asd" x


idLin :: a ->. a
idLin x = x

asdasd :: a ->. a
asdasd = id idLin

foo :: Bool -> (Int ->. Bool) ->. Bool
foo True f = idLin (f 10)
foo False f = idLin (f 10)

foo' :: Bool -> (Int ->. Bool) ->. Bool
foo' b f = case b of
  True -> idLin (f 10)
  False -> idLin (f 10)

data Dict (c :: Constraint) where
  Dict :: c =>. Dict c

output :: Dict (?foo :: Int)
output = let ?foo = (10 :: Int) in Dict

-- what :: Dict (?foo :: Int) a ->. (Int, Int)
-- what (Dict i) = (?foo, 10)

good4 :: (?counter :: Int) =>. Dict (?counter :: Int)
good4 = let ?counter = ?counter + 1 in Dict

get :: forall (s :: Symbol) (t :: Type). IP s t =>. Dict (IP s t)
get = Dict

modify :: (?counter :: Int) =>. Dict (?counter :: Bool)
modify = let ?counter = False in Dict

consume :: (?foo :: Int) =>. Int
consume = ?foo

test :: (?counter :: Int) =>. Bool
test = case get @"counter" of
  Dict -> case get @"counter" @Int of
    Dict -> case modify of
      Dict -> case get @"counter" @Bool of
        Dict -> ?counter

-- bad4 :: (?foo :: Int) =>. Dict (?foo :: Int) Int
-- bad4 = Dict ?foo

data IOL c a = IOL {runIOL :: RealWorld -> (RealWorld, a .<= c)}

(>>=) :: IOL c a -> (c =>. a -> IOL d b) -> IOL d b
io_a >>= f = IOL $ \rw -> case runIOL io_a rw of
                            (rw', Pack a) -> runIOL (f a) rw'

return :: forall c a. c =>. a -> IOL c a
return a = IOL (\rw -> (rw, Pack a))

readTwoLines :: (?counter :: Int) =>.  IOL (?counter :: Int) ()
readTwoLines = do
  return @(?counter :: Int) 10 >>= \b ->
    return @(?counter :: Int) ()

-- openFile :: FilePath -> IOL (Handle h .<= Open h)
-- openFile = undefined

-- close :: Open h =>. Handle h -> IOL ()
-- close = undefined

-- readLine :: Open h =>. Handle h -> IOL (String .<= Open h)
-- readLine = undefined

-- data Plot r where
--   -- | Set the label of something labellable
--   Label :: Plot (Texted ': r) -> Plot r
--   Text :: Has Texted r => T.Text -> Plot r
--   -- | Set the title of a canvas
--   Title :: Plot (Texted ': r) -> Plot r

data Canvas = Canvas

set :: forall (s :: Symbol) a . IP s () =>. a -> Canvas -> Canvas
set _ x = x

done :: (?title :: (), ?name :: ()) =>. Canvas -> Canvas
done = set @"title" "foo"
     . set @"name" 10

bottom :: a
bottom = bottom

data Loc a l = Loc
data Ptr l = Ptr

data a .<= c where
  Pack :: c =>. a -> a .<= c

ptrGet :: IP name (Loc a l) =>. Ptr l -> a .<= IP name (Loc a l)
ptrGet _ = Pack bottom

ptrSet :: forall name a l x. IP name (Loc x l) =>. Ptr l -> a -> Dict (IP name (Loc a l))
ptrSet _ _ = unsafeCoerce (let ?pf = Loc in Dict @(?pf :: Loc a l))

free :: IP name (Loc a l) =>. Ptr l -> ()
free _ = ()

data Uninitialised a

alloc :: forall name a l. Ptr l .<= IP name (Loc (Uninitialised a) l)
alloc = unsafeCoerce (let ?pf = Loc in Pack @(?pf :: Loc a l) Ptr)

swap :: (?pf1 :: Loc a l1, ?pf2 :: Loc a l2) =>. Ptr l1 -> Ptr l2 -> (Dict (?pf1 :: Loc a l1), Dict (?pf2 :: Loc a l2))
swap ptr1 ptr2
  = case (ptrGet @"pf1" ptr1, ptrGet @"pf2" ptr2) of
      (Pack x, Pack y) -> case (ptrSet @"pf1" ptr1 y, ptrSet @"pf2" ptr2 x) of
        (Dict, Dict) -> (Dict, Dict)

-- swappy :: forall l1 l2. Int
-- swappy = case (alloc @"pf1" @Int @l1, alloc @"pf2" @Int @l2) of
--   (Pack ptr1, Pack ptr2) -> case (ptrGet @"pf1" ptr1, ptrGet @"pf2" ptr2) of
--     (Pack v1, Pack v2) -> v1 + v2
-- examples/Ex1.hs:214:27: error:
--     • Couldn't match expected type ‘Int’
--                   with actual type ‘Uninitialised Int’

swappy :: forall l1 l2. ()
swappy = case (alloc @"pf1" @Int @l1, alloc @"pf2" @Int @l2) of
  (Pack ptr1, Pack ptr2) -> case (ptrSet @"pf1" ptr1 10, ptrSet @"pf2" ptr2 20) of
    (Dict, Dict) -> case swap ptr1 ptr2 of
      (Dict, Dict) -> free @"pf1" ptr1 `seq` free @"pf2" ptr2


stateful :: (?pf1 :: Loc Int l1) =>. Ptr l1 -> () .<= (?pf1 :: Loc Bool l1)
stateful ptr = case ptrSet @"pf1" ptr False of
  Dict -> Pack ()

-- shadowing semantics of implicit parameters
-- allows us to thread proofs around linearly
