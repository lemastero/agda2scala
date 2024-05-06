module PrintScala2Test ( printScala2Tests ) where

import Test.HUnit ( Test(..), assertEqual )
import Agda.Compiler.Scala.PrintScala2 (
  printScala2
  , printSealedTrait
  , printCaseObject
  , printPackage
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
  (assertEqual "printPackage"
    "object adts"
    (printPackage "adts"))

testCombineLines :: Test
testCombineLines = TestCase
  (assertEqual "combineLines"
    "a\nb"
    (combineLines ["", "a", "", "", "b", "", "", ""]))

testPrintScala2:: Test
testPrintScala2 = TestCase
  (assertEqual "printScala2" (printScala2 $ SePackage "adts" moduleContent)
  "object adts {\n\nsealed trait Rgb\ncase object Red extends Rgb\ncase object Green extends Rgb\ncase object Blue extends Rgb\n\nsealed trait Color\ncase object Light extends Color\ncase object Dark extends Color\n}\n"
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
    

printScala2Tests :: Test
printScala2Tests = TestList [
  TestLabel "printCaseObject" testPrintCaseObject
  , TestLabel "printSealedTrait" testPrintSealedTrait
  , TestLabel "printPackage" testPrintPackage
  , TestLabel "combineLines" testCombineLines
  , TestLabel "printCaseClass" testPrintCaseClass
  , TestLabel "printScala2" testPrintScala2
  ]