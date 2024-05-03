module Main (main) where

import Test.HUnit (
  Test(..)
  , failures
  , runTestTT
  )
import System.Exit ( exitFailure , exitSuccess )

import PrintScalaExprTest ( printScalaTests )
import ScalaBackendTest ( backendTests )

allTests :: Test
allTests = TestList [ backendTests , printScalaTests ]

main :: IO ()
main = do
  result <- runTestTT allTests
  if (failures result) > 0 then exitFailure else exitSuccess
