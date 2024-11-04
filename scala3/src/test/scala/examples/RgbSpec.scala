package examples

import zio.test.Assertion.equalTo
import zio.test.junit.JUnitRunnableSpec
import zio.test.{ assert, suite, test }

import examples.adts.{ Rgb, Red, Green, Blue, idRgb }
import examples.adts.{ RgbPair, constRgbPair }
import examples.adts.{ Bool, True, False }

object RgbSpec extends JUnitRunnableSpec:

  def spec = suite("Test Rgb")(
    test("identity on Rgb") {
      assert(idRgb(Red))(equalTo(Red))
      assert(idRgb(Green))(equalTo(Green))
      assert(idRgb(Blue))(equalTo(Blue))
    },
    test ("const on RgbPair") {
      assert( constRgbPair(Red,   RgbPair(True, Blue) ) )(equalTo( RgbPair(True, Blue) ))
      assert( constRgbPair(Green, RgbPair(False, Red) ) )(equalTo( RgbPair(False, Red) ))
      assert( constRgbPair(Blue,  RgbPair(True, Blue) ) )(equalTo( RgbPair(True, Blue) ))
    }
  )
