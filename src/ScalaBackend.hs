module ScalaBackend (
  backend
  , runScalaBackend
  , scalaBackend
  ) where

import Agda.Main ( runAgda )
import Agda.Compiler.Backend

runScalaBackend :: IO ()
runScalaBackend = runAgda [backend]

backend :: Backend
backend = Backend scalaBackend

type ScalaFlags = ()
type ScalaEnv = ()
type ScalaModuleEnv = ()
type ScalaModule = ()
type ScalaDefinition = ()

scalaBackend :: Backend' ScalaFlags ScalaEnv ScalaModuleEnv ScalaModule ScalaDefinition
scalaBackend = Backend'
  { backendName           = "Scala"
  , backendVersion        = Just "0.1"
  , options               = ()
  , commandLineFlags      = []
  , isEnabled             = const True
  , preCompile            = \ opt -> return opt
  , postCompile           = \ _ _ _ -> return ()
  , preModule             = \ _ _ _ _ -> return $ Recompile ()
  , postModule            = \ _ _ _ _ _ -> return ()
  , compileDef            = \ _ _ _ _ -> return ()
  , scopeCheckingSuffices = False
  , mayEraseType          = const $ return True
  }
