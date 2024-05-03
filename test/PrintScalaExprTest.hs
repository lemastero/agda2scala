module PrintScalaExprTest ( printScalaTests ) where

import Test.HUnit ( Test(..), assertEqual )
import Agda.Compiler.Scala.PrintScalaExpr (
  printScalaExpr
  , printSealedTrait
  , printCaseObject
  , printPackage
  , combineLines
  )
import Agda.Compiler.Scala.ScalaExpr ( ScalaExpr(..) )

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
    "package adts"
    (printPackage "adts"))

testCombineLines :: Test
testCombineLines = TestCase
  (assertEqual "combineLines"
    "a\nb"
    (combineLines ["", "a", "", "", "b", "", "", ""]))

testPrintScalaExpr :: Test
testPrintScalaExpr = TestCase
  (assertEqual "printScalaExpr" (printScalaExpr $ SePackage "adts" moduleContent)
  "package adts\n\nsealed trait Rgb\ncase object Red extends Rgb\ncase object Green extends Rgb\ncase object Blue extends Rgb\n\nsealed trait Color\ncase object Light extends Color\ncase object Dark extends Color\n"
  )
  where
    moduleContent = [rgbAdt, blank, blank, blank, colorAdt, blank, blank]
    rgbAdt = SeAdt "Rgb" ["Red","Green","Blue"]
    colorAdt = SeAdt "Color" ["Light","Dark"]
    blank = Unhandled "" ""

printScalaTests :: Test
printScalaTests = TestList [
  TestLabel "printCaseObject" testPrintCaseObject
  , TestLabel "printSealedTrait" testPrintSealedTrait
  , TestLabel "printPackage" testPrintPackage
  , TestLabel "combineLines" testCombineLines
  , TestLabel "printScalaExpr" testPrintScalaExpr
  ]
