{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnsaturatedTypeFamilies #-}

module UnsaturatedArrowParse where

type family (<<<) (f :: b ~> c) (g :: a ~> b) (x :: a) :: c where
  (f <<< g) x = f (g x)
infixr 9 <<<
