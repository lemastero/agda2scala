object adts {

sealed trait Rgb
case object Red extends Rgb
case object Green extends Rgb
case object Blue extends Rgb

sealed trait Bool
case object True extends Bool
case object False extends Bool

def idRgb(x: Rgb): Rgb = x

final case class RgbPair(snd: Bool, fst: Rgb)
}
