module PrintScala3Test ( printScala3Tests ) where

import Test.HUnit ( Test(..), assertEqual )
import Agda.Compiler.Scala.PrintScala3 (
  printScala3
  , printSealedTrait
  , printCaseObject
  , printPackageAndObject
  , combineLines
  , printCaseClass
  )
import Agda.Compiler.Scala.ScalaExpr ( ScalaExpr(..), SeVar(..) )

testPrintCaseObject :: Test
testPrintCaseObject = TestCase
  (assertEqual "printCaseObject"
    "case object Light extends Color"
    (printCaseObject "Color" "Light"))

testPrintSealedTrait :: Test
testPrintSealedTrait = TestCase
  (assertEqual "printSealedTrait"
    "sealed trait Color"
    (printSealedTrait "Color"))

testPrintPackage :: Test
testPrintPackage = TestCase
  (assertEqual "printPackageAndObject"
    "object adts"
    (printPackageAndObject ["adts"]))

testCombineLines :: Test
testCombineLines = TestCase
  (assertEqual "combineLines"
    "a\nb"
    (combineLines ["", "a", "", "", "b", "", "", ""]))

testPrintScala3 :: Test
testPrintScala3 = TestCase
  (assertEqual "printScala3"
    "object adts:\n  enum Rgb:\n    case Red\n    case Green\n    case Blue\n\n  enum Color:\n    case Light\n    case Dark\n"
    (printScala3 $ SePackage ["adts"] moduleContent)
  )
  where
    moduleContent = [rgbAdt, blank, blank, blank, colorAdt, blank, blank]
    rgbAdt = SeSum "Rgb" ["Red","Green","Blue"]
    colorAdt = SeSum "Color" ["Light","Dark"]
    blank = Unhandled "" ""

testPrintCaseClass :: Test
testPrintCaseClass = TestCase
  (assertEqual "printCaseClass"
    "final case class RgbPair(snd: Bool, fst: Rgb)"
    (printCaseClass "RgbPair" [SeVar "snd" "Bool", SeVar "fst" "Rgb"]))


printScala3Tests :: Test
printScala3Tests = TestList [
  TestLabel "printCaseObject" testPrintCaseObject
  , TestLabel "printSealedTrait" testPrintSealedTrait
  , TestLabel "printPackage" testPrintPackage
  , TestLabel "combineLines" testCombineLines
  , TestLabel "printCaseClass" testPrintCaseClass
  , TestLabel "printScala3" testPrintScala3
  ]
