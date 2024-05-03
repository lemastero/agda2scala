module ScalaBackendTest ( backendTests ) where

import Test.HUnit ( Test(..), assertEqual )
import Agda.Compiler.Backend ( isEnabled )
import Agda.Compiler.Scala.Backend ( scalaBackend', defaultOptions )

testIsEnabled :: Test
testIsEnabled = TestCase
  (assertEqual "isEnabled" (isEnabled scalaBackend' defaultOptions) True)

backendTests :: Test
backendTests = TestList [TestLabel "scalaBackend" testIsEnabled]
