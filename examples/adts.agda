module examples.adts where

-- simple sum type no arguments - sealed trait + case objects
data Rgb : Set where
  Red : Rgb
  Green : Rgb
  Blue : Rgb
{-# COMPILE AGDA2SCALA Rgb #-}

-- simple sum type with arguments - sealed trait + case class

data Color : Set where
  Light : Rgb -> Color
  Dark : Rgb -> Color
{-# COMPILE AGDA2SCALA Color #-}
