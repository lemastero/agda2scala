module PrintScala2Test ( printScala2Tests ) where

import Test.HUnit ( Test(..), assertEqual )
import Agda.Compiler.Scala.PrintScala2 (
  printScala2
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

testObjectWhenNoPackage :: Test
testObjectWhenNoPackage = TestCase
  (assertEqual "printPackageAndObject"
    "object adts"
    (printPackageAndObject ["adts"]))

testPrintPackageAndObject :: Test
testPrintPackageAndObject = TestCase
  (assertEqual "printPackageAndObject"
    "package example\n\nobject adts"
    (printPackageAndObject ["example", "adts"]))

testPrintMultiplePartPackageAndObject :: Test
testPrintMultiplePartPackageAndObject = TestCase
  (assertEqual "printPackageAndObject"
    "package org.example\n\nobject adts"
    (printPackageAndObject ["org", "example", "adts"]))

testCombineLines :: Test
testCombineLines = TestCase
  (assertEqual "combineLines"
    "a\nb"
    (combineLines ["", "a", "", "", "b", "", "", ""]))

testPrintScala2:: Test
testPrintScala2 = TestCase
  (assertEqual "printScala2"
  "object adts {\n\nsealed trait Rgb\ncase object Red extends Rgb\ncase object Green extends Rgb\ncase object Blue extends Rgb\n\nsealed trait Color\ncase object Light extends Color\ncase object Dark extends Color\n}\n"
  (printScala2 $ SePackage ["adts"] moduleContent)
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
  , TestLabel "printObject" testObjectWhenNoPackage
  , TestLabel "printPackageAndObject" testPrintPackageAndObject
  , TestLabel "printPackageAndObject 2" testPrintMultiplePartPackageAndObject
  , TestLabel "combineLines" testCombineLines
  , TestLabel "printCaseClass" testPrintCaseClass
  , TestLabel "printScala2" testPrintScala2
  ]
