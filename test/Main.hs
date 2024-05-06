module Main (main) where

import Test.HUnit (
  Test(..)
  , failures
  , runTestTT
  )
import System.Exit ( exitFailure , exitSuccess )

import PrintScala2Test ( printScala2Tests )
import PrintScala3Test ( printScala3Tests )
import ScalaBackendTest ( backendTests )

allTests :: Test
allTests = TestList [
  backendTests
  , printScala2Tests
  , printScala3Tests
  ]

main :: IO ()
main = do
  result <- runTestTT allTests
  if (failures result) > 0 then exitFailure else exitSuccess
