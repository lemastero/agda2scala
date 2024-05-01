module Main (main) where

import Agda.Compiler.Scala.Backend ( scalaBackend', defaultOptions )
import Test.HUnit (
  Test(..)
  , assertEqual
  , failures
  , runTestTT)
import System.Exit ( exitFailure , exitSuccess )
import Agda.Compiler.Backend ( isEnabled )

testIsEnabled :: Test
testIsEnabled = TestCase
  (assertEqual "isEnabled" (isEnabled scalaBackend' defaultOptions) True)

tests :: Test
tests = TestList [TestLabel "scalaBackend" testIsEnabled]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then exitFailure else exitSuccess
