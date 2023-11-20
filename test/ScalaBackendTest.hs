module Main (main) where

import ScalaBackend (
   scalaBackend )
import Test.HUnit
import qualified System.Exit as Exit
import Agda.Compiler.Backend

testIsEnabled :: Test
testIsEnabled = TestCase
  (assertEqual "isEnabled" (isEnabled scalaBackend ()) True)

tests :: Test
tests = TestList [TestLabel "scalaBackend" testIsEnabled]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
