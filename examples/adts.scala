object adts {

sealed trait Rgb
case object Red extends Rgb
case object Green extends Rgb
case object Blue extends Rgb

sealed trait Bool
case object True extends Bool
case object False extends Bool

final case class RgbPair(snd: Bool, fst: Rgb)

def idRgb(theArg: Rgb): Rgb = theArg

def and0(rgbArg: Rgb, rgbPairArg: RgbPair): RgbPair = rgbPairArg
}
