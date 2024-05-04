module examples.adts where

-- simple product type no arguments - sealed trait + case objects

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

-- simple sum type - case class

record RgbPair : Set where
  constructor mkRgbPair
  field
    fst : Rgb
    snd : Bool
{-# COMPILE AGDA2SCALA RgbPair #-}
