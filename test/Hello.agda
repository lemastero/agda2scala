{-# OPTIONS --cubical-compatible #-}

module test.Hello where

-- Type with two inhabitants
data Bool : Set where
  false true : Bool
{-# COMPILE AGDA2SCALA Bool #-}

{- Logical connective not - negation -}
not : Bool -> Bool
not true  = false
not false = true
