module PrintScalaExprTest ( printScalaTests ) where

import Test.HUnit ( Test(..), assertEqual )
import Agda.Compiler.Scala.PrintScalaExpr (
  printSealedTrait
  , printCaseObject
  , printPackage
  )

testPrintCaseObject :: Test
testPrintCaseObject = TestCase
  (assertEqual "printCaseObject" (printCaseObject "Color" "Light") "case object Light extends Color")
  
testPrintSealedTrait :: Test
testPrintSealedTrait = TestCase
  (assertEqual "printSealedTrait" (printSealedTrait "Color") "sealed trait Color")

testPrintPackage :: Test
testPrintPackage = TestCase
  (assertEqual "printPackage" (printPackage "adts") "package adts")

printScalaTests :: Test
printScalaTests = TestList [
  TestLabel "printCaseObject" testPrintCaseObject
  , TestLabel "printSealedTrait" testPrintSealedTrait
  , TestLabel "printPackage" testPrintPackage
  ]
