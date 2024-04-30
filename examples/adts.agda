module examples.adts where

-- simple sum type no arguments - sealed trait + case objects
data Rgb : Set where
  Red : Rgb
  Green : Rgb
  Blue : Rgb
{-# COMPILE AGDA2SCALA Rgb #-}

data Bool : Set where
   True : Bool
   False : Bool
{-# COMPILE AGDA2SCALA Bool #-}

-- trivial function with single argument

idRgb : Rgb -> Rgb
idRgb x = x
{-# COMPILE AGDA2SCALA idRgb #-}
