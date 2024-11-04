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

-- simple sum type with arguments - sealed trait + case class

data Color : Set where
  Light : Rgb -> Color
  Dark : Rgb -> Color
-- TODO {-# COMPILE AGDA2SCALA Color #-}

-- simple sum type - case class

record RgbPair : Set where
  constructor mkRgbPair
  field
    fst : Rgb
    snd : Bool
{-# COMPILE AGDA2SCALA RgbPair #-}

-- trivial function with single argument

idRgb : Rgb -> Rgb
idRgb theArg = theArg
{-# COMPILE AGDA2SCALA idRgb #-}

-- const function with one named argument

rgbConstTrue1 : (rgb : Rgb) → Bool
rgbConstTrue1 rgb = True -- TODO produce function body
-- TODO {-# COMPILE AGDA2SCALA rgbConstTrue1 #-}

-- function with multiple named arguments

constRgbPair : (rgbPairArg : RgbPair) -> (rgbArg : Rgb) -> RgbPair
constRgbPair rgbPairArg rgbArg = rgbPairArg
{-# COMPILE AGDA2SCALA constRgbPair #-}
