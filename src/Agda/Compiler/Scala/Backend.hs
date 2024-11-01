module Agda.Compiler.Scala.Backend (
  runScalaBackend
  , scalaBackend
  , scalaBackend'
  , defaultOptions
  ) where

import Control.DeepSeq ( NFData(..) )
import Control.Monad ( unless )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import qualified Data.List.NonEmpty as Nel
import Data.Maybe ( fromMaybe )
import Data.Map ( Map )
import qualified Data.Text.IO as T
import Data.Version ( showVersion )
import System.Console.GetOpt ( OptDescr(Option), ArgDescr(ReqArg) )

import Paths_agda2scala ( version )

import Agda.Main ( runAgda )
import Agda.Compiler.Backend
import Agda.Interaction.Options ( OptDescr )
import Agda.Compiler.Common ( curIF, compileDir )
import Agda.Syntax.Abstract.Name ( QName )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Common ( moduleNameParts )
import Agda.Syntax.Internal ( qnameModule )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName, moduleNameToFileName )
import Agda.TypeChecking.Monad

import Agda.Compiler.Scala.ScalaExpr ( ScalaName, ScalaExpr(..), unHandled )
import Agda.Compiler.Scala.AgdaToScalaExpr ( compileDefn )
import Agda.Compiler.Scala.PrintScala2 ( printScala2 )
import Agda.Compiler.Scala.PrintScala3 ( printScala3 )

runScalaBackend :: IO ()
runScalaBackend = runAgda [scalaBackend]

scalaBackend :: Backend
scalaBackend = Backend scalaBackend'

data Options = Options {
  optOutDir :: Maybe FilePath,
  scalaDialect :: Maybe String
} deriving (Show)

instance NFData Options where
  rnf _ = ()

type ScalaFlags = Options
type ScalaEnv = Options
type ScalaModuleEnv = ()
type ScalaModule = ()
type ScalaDefinition = ScalaExpr

{- Backend contains implementations of hooks called around compilation of Agda code -}
scalaBackend' :: Backend' ScalaFlags ScalaEnv ScalaModuleEnv ScalaModule ScalaDefinition
scalaBackend' = Backend'
  { backendName           = "agda2scala"
  , backendVersion        = scalaBackendVersion
  , options               = defaultOptions
  , commandLineFlags      = scalaCmdLineFlags
  , isEnabled             = const True
  , preCompile            = return
  , compileDef            = scalaCompileDef
  , postCompile           = scalaPostCompile
  , preModule             = scalaPreModule
  , postModule            = scalaPostModule
  , scopeCheckingSuffices = False
  , mayEraseType          = const $ return True
  }

scalaBackendVersion :: Maybe String
scalaBackendVersion = Just (showVersion version)

defaultOptions :: ScalaFlags
defaultOptions = Options{ optOutDir = Nothing, scalaDialect = Nothing }

-- TODO add option to choose Scala version (Scala 2.12 vs dotty vs Scala 4)
-- TODO perhaps add option to choose if we want to produce Functor, Monad etc from zio/zio-prelude or typelevel/cats-effect
-- TODO perhaps add option to use annotations from siddhartha-gadgil/ProvingGround library
scalaCmdLineFlags :: [OptDescr (Flag ScalaFlags)]
scalaCmdLineFlags = [
  Option
    ['o'] ["out-dir"] (ReqArg outDirOpt "DIR")
    "Write output files to DIR. (default: project root)",
  Option
    ['b'] ["scala-dialect"] (ReqArg scalaDialectOpt "scalaDialect")
    "Write output files using Scala2 or Scala3 dialect. (default: Scala2)"
  ]

outDirOpt :: Monad m => FilePath -> Options -> m Options
outDirOpt dir opts = return opts{ optOutDir = Just dir }

scalaDialectOpt :: Monad m => String -> Options -> m Options
scalaDialectOpt sVer opts = return opts{ scalaDialect = Just sVer }

scalaCompileDef :: ScalaEnv
  -> ScalaModuleEnv
  -> IsMain
  -> Definition
  -> TCM ScalaDefinition
scalaCompileDef _ _ isMain Defn{theDef = theDef, defName = defName}
  = withCurrentModule (qnameModule defName)
  $ getUniqueCompilerPragma "AGDA2SCALA" defName >>= handlePragma defName theDef
  
handlePragma :: QName -> Defn -> Maybe CompilerPragma -> TCMT IO ScalaDefinition
handlePragma defName theDef pragma = case pragma of
  Nothing -> return $ Unhandled "" ""
  Just (CompilerPragma _ _) -> 
    return $ compileDefn defName theDef

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
scalaPreModule _ _ _ _ = do
  setScope . iInsideScope =<< curIF
  return $ Recompile ()

scalaPostModule :: ScalaEnv
  -> ScalaModuleEnv
  -> IsMain
  -> TopLevelModuleName
  -> [ScalaDefinition]
  -> TCM ScalaModule
scalaPostModule env modEnv isMain mName cdefs = do
  outDir <- compileDir
  compileLog $ "compiling " <> mkOutFile outDir
  unless (all unHandled cdefs)
    $ liftIO
    $ writeFile (mkOutFile outDir) fileContent
  where
    fileName = scalaFileName mName
    dirName outDir = fromMaybe outDir (optOutDir env)
    mkOutFile outDir = (dirName outDir) <> "/" <> fileName
    scalaExprs = compileModule mName cdefs
    fileContent = printer scalaExprs
    printer = case (scalaDialect env) of
     (Just "Scala3") -> printScala3
     (Just "scala2") -> printScala2 
     Nothing         -> printScala2

scalaFileName :: TopLevelModuleName -> FilePath
scalaFileName mName = moduleNameToFileName mName "scala"

compileModule :: TopLevelModuleName -> [ScalaDefinition] -> ScalaDefinition
compileModule mName cdefs =
  SePackage (moduleName mName) cdefs

moduleName :: TopLevelModuleName -> String
moduleName n = prettyShow (Nel.last (moduleNameParts n))

compileLog :: String -> TCMT IO ()
compileLog msg = liftIO $ putStrLn msg
