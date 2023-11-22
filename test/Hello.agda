{-# OPTIONS --cubical-compatible --safe #-}

module test.Hello where

-- Type with two inhabitants
data Bool : Set where
  false true : Bool

{- Logical connective not - negation -}
not : Bool -> Bool
not true  = false
not false = true
