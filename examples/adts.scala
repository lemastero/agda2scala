package adts

sealed trait Rgb
case object Red extends Rgb
case object Green extends Rgb
case object Blue extends Rgb

sealed trait Color
case object Light extends Color
case object Dark extends Color
