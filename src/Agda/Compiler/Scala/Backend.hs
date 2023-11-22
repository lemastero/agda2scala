module Agda.Compiler.Scala.Backend (
  runScalaBackend
  , scalaBackend
  , scalaBackend'
  ) where

import Agda.Main ( runAgda )
import Agda.Compiler.Backend (
  Backend(..)
  , Backend'(..)
  , Definition
  , Flag
  , IsMain
  , Recompile(..)
  , TCM
  )
import Agda.Interaction.Options ( OptDescr )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName )

runScalaBackend :: IO ()
runScalaBackend = runAgda [scalaBackend]

scalaBackend :: Backend
scalaBackend = Backend scalaBackend'

type ScalaFlags = ()
type ScalaEnv = ()
type ScalaModuleEnv = ()
type ScalaModule = ()
type ScalaDefinition = ()

scalaBackend' :: Backend' ScalaFlags ScalaEnv ScalaModuleEnv ScalaModule ScalaDefinition
scalaBackend' = Backend'
  { backendName           = "agda2scala"
  , backendVersion        = Just "0.1"
  , options               = ()
  , commandLineFlags      = scalaCmdLineFlags
  , isEnabled             = const True
  , preCompile            = \ opt -> return opt
  , postCompile           = \ _ _ _ -> return ()
  , preModule             = \ _ _ _ _ -> return $ Recompile ()
  , postModule            = scalaPostModule
  , compileDef            = scalaCompileDef
  , scopeCheckingSuffices = False
  , mayEraseType          = const $ return True
  }

scalaCmdLineFlags :: [OptDescr (Flag ScalaFlags)]
scalaCmdLineFlags = []

scalaPostModule :: ScalaEnv
  -> ScalaModuleEnv
  -> IsMain
  -> TopLevelModuleName
  -> [ScalaDefinition]
  -> TCM ScalaModule
scalaPostModule = \ _ _ _ _ _ -> return ()

scalaCompileDef :: ScalaEnv -> ScalaModuleEnv -> IsMain -> Definition -> TCM ScalaDefinition
scalaCompileDef = \ _ _ _ _ -> return ()
