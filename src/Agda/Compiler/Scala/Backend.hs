module Agda.Compiler.Scala.Backend (
  runScalaBackend
  , scalaBackend
  , scalaBackend'
  ) where

import Data.Map ( Map )
import Agda.Main ( runAgda )
import Agda.Compiler.Backend (
  Backend(..)
  , Backend'(..)
  , Definition
  , Flag
  , IsMain
  , Recompile(..)
  , TCM )
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
type ScalaDefinition = (IsMain, Definition)

{- Backend contains implementations of hooks called around compilation of Agda code

type checking:
=============

compilation:
============ 
  
   -> preModule
   module Foo compilation:
   ----------------------
     -> preCompile
     definition Foo 1 compilation:
       -> compileDef
     -> postCompile

     -> preCompile
     definition Foo 2 compilation:
        -> compileDef
     -> postCompile
    
     ...
   -> postModule

   -> preModule
   module Bar compilation:
   ----------------------
   ...
   -> postModule

  ...
-}
scalaBackend' :: Backend' ScalaFlags ScalaEnv ScalaModuleEnv ScalaModule ScalaDefinition
scalaBackend' = Backend'
  { backendName           = "agda2scala"
  , backendVersion        = scalaBackendVersion
  , options               = defaultOptions
  , commandLineFlags      = scalaCmdLineFlags
  , isEnabled             = const True
  , preCompile            = scalaPreCompile
  , compileDef            = scalaCompileDef
  , postCompile           = scalaPostCompile
  , preModule             = scalaPreModule
  , postModule            = scalaPostModule
  , scopeCheckingSuffices = False
  , mayEraseType          = const $ return True
  }

-- TODO get version from cabal definition, perhaps git hash too
scalaBackendVersion :: Maybe String
scalaBackendVersion = Just "0.1"

defaultOptions :: ScalaFlags
defaultOptions = ()

-- TODO add option to choose Scala version (Scala 2.12 vs dotty vs Scala 4)
-- TODO perhaps add option to choose if we want to produce Functor, Monad etc from zio/zio-prelude or typelevel/cats-effect
-- TODO perhaps add option to use annotations from siddhartha-gadgil/ProvingGround library
scalaCmdLineFlags :: [OptDescr (Flag ScalaFlags)]
scalaCmdLineFlags = []

scalaPreCompile :: ScalaFlags -> TCM ScalaEnv
scalaPreCompile opt = return opt

-- TODO perhaps transform definitions here, ATM just pass it with extra information is it main
-- Rust backend perform transformation to Higher IR
-- Scheme pass as is (like here)
scalaCompileDef :: ScalaEnv
  -> ScalaModuleEnv
  -> IsMain
  -> Definition
  -> TCM ScalaDefinition
scalaCompileDef _ _ isMain def = return (isMain, def)

scalaPostCompile :: ScalaEnv
  -> IsMain
  -> Map TopLevelModuleName ScalaModule
  -> TCM ()
scalaPostCompile _ _ _ = return ()

scalaPreModule :: ScalaEnv
  -> IsMain
  -> TopLevelModuleName
  -> Maybe FilePath
  -> TCM (Recompile ScalaModuleEnv ScalaModule)
scalaPreModule _ _ _ _ = return $ Recompile ()

-- TODO implement translation here
scalaPostModule :: ScalaEnv
  -> ScalaModuleEnv
  -> IsMain
  -> TopLevelModuleName
  -> [ScalaDefinition]
  -> TCM ScalaModule
scalaPostModule _ _ _ _ _ = return ()

