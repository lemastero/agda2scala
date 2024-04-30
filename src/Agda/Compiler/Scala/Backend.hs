{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Agda.Compiler.Scala.Backend (
  runScalaBackend
  , scalaBackend
  , scalaBackend'
  , defaultOptions
  ) where

import Control.DeepSeq ( NFData(..) )
import Data.Map ( Map )
import qualified Data.Text.IO as T

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

import Control.Monad ( unless )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import qualified Data.List.NonEmpty as Nel
import Data.Maybe ( fromMaybe )
import Data.Version ( showVersion )
import System.Console.GetOpt ( OptDescr(Option), ArgDescr(ReqArg) )

import Paths_agda2scala ( version )

import Agda.Compiler.Common ( curIF, compileDir )
import Agda.Compiler.Backend ( IsMain, Defn(..) )
import Agda.Syntax.Abstract.Name ( QName )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Common ( Arg(..), ArgName, Named(..), moduleNameParts )
import Agda.Syntax.Internal (
  Clause(..), DeBruijnPattern, DBPatVar(..), Dom(..), unDom, PatternInfo(..), Pattern'(..),
  qnameName, qnameModule, Telescope, Tele(..), Term(..), Type, Type''(..) )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName, moduleNameToFileName )
import Agda.TypeChecking.Monad.Base ( Definition(..) )
import Agda.TypeChecking.Monad
import Agda.TypeChecking.CompiledClause ( CompiledClauses(..), CompiledClauses'(..) )

import Agda.Compiler.Scala.Expr ( ScalaName, ScalaExpr(..), unHandled )

runScalaBackend :: IO ()
runScalaBackend = runAgda [scalaBackend]

scalaBackend :: Backend
scalaBackend = Backend scalaBackend'

data Options = Options { optOutDir :: Maybe FilePath }

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
defaultOptions = Options{ optOutDir = Nothing }

-- TODO add option to choose Scala version (Scala 2.12 vs dotty vs Scala 4)
-- TODO perhaps add option to choose if we want to produce Functor, Monad etc from zio/zio-prelude or typelevel/cats-effect
-- TODO perhaps add option to use annotations from siddhartha-gadgil/ProvingGround library
scalaCmdLineFlags :: [OptDescr (Flag ScalaFlags)]
scalaCmdLineFlags = [
  Option ['o'] ["out-dir"] (ReqArg outDirOpt "DIR")
         "Write output files to DIR. (default: project root)"
  ]

outDirOpt :: Monad m => FilePath -> Options -> m Options
outDirOpt dir opts = return opts{ optOutDir = Just dir }

scalaCompileDef :: ScalaEnv
  -> ScalaModuleEnv
  -> IsMain
  -> Definition
  -> TCM ScalaDefinition
scalaCompileDef _ _ isMain Defn{..}
  = withCurrentModule (qnameModule defName)
  $ getUniqueCompilerPragma "AGDA2SCALA" defName >>= \case
      Nothing -> return $ Unhandled "compile" ""
      Just (CompilerPragma _ _) -> 
        return $ compileDefn defName theDef

compileDefn :: QName -> Defn -> ScalaDefinition
compileDefn defName theDef = case theDef of 
    Datatype{dataCons = fields} ->
      compileDataType defName fields
    Function{funCompiled = funDef, funClauses = fc} ->
      Unhandled "compileDefn Function" (show defName ++ "\n = \n" ++ show theDef)
    RecordDefn(RecordData{_recFields = recFields, _recTel = recTel}) ->
      Unhandled "compileDefn RecordDefn" (show defName ++ "\n = \n" ++ show theDef)
    other ->
      Unhandled "compileDefn other" (show defName ++ "\n = \n" ++ show theDef)

compileDataType :: QName -> [QName] -> ScalaDefinition
compileDataType defName fields = SeAdt (showName defName) (map showName fields)
-- Unhandled "compileDefn Datatype" (show defName ++ "\n = \n" ++ show theDef)

showName :: QName -> ScalaName
showName = prettyShow . qnameName

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
  compileLog $ "compiling " <> (outFile outDir)
  unless (all unHandled cdefs) $ liftIO
    $ writeFile (outFile outDir)
    $ prettyPrintScalaExpr (compileModule mName cdefs)
  where
    fileName = scalaFileName mName
    dirName outDir = fromMaybe outDir (optOutDir env)
    outFile outDir = (dirName outDir) <> "/" <> fileName

scalaFileName :: TopLevelModuleName -> FilePath
scalaFileName mName = moduleNameToFileName mName "scala"

compileModule :: TopLevelModuleName -> [ScalaDefinition] -> ScalaDefinition
compileModule mName cdefs =
  SePackage (moduleName mName) cdefs

moduleName :: TopLevelModuleName -> String
moduleName n = prettyShow (Nel.last (moduleNameParts n))

compileLog :: String -> TCMT IO ()
compileLog msg = liftIO $ putStrLn msg

prettyPrintScalaExpr :: ScalaDefinition -> String
prettyPrintScalaExpr def = case def of
  (SePackage pName defs) ->
    moduleHeader pName
    <> defsSeparator <> (
      defsSeparator -- empty line before first definition in package
      <> combineLines (map prettyPrintScalaExpr defs))
    <> defsSeparator
  (SeAdt adtName adtCases) ->
    "sealed trait" <> exprSeparator <> adtName
    <> defsSeparator
    <> unlines (map (prettyPrintCaseObject adtName) adtCases)
  -- TODO not sure why I get this
  -- (Unhandled name payload) -> "TODO " ++ (show name) ++ " " ++ (show payload)
  (Unhandled name payload) -> ""
  -- XXX at the end there should be no Unhandled expression
  -- other -> "unsupported prettyPrintScalaExpr " ++ (show other)


prettyPrintCaseObject :: ScalaName -> ScalaName -> String
prettyPrintCaseObject superName xs =
  "case object" <> exprSeparator <> xs <> exprSeparator <> "extends" <> exprSeparator <> superName

moduleHeader :: String -> String
moduleHeader pName = "package" <> exprSeparator <> pName <> exprSeparator

bracket :: String -> String
bracket str = "{\n" <> str <> "\n}"

defsSeparator :: String
defsSeparator = "\n"

exprSeparator :: String
exprSeparator = " "

combineLines :: [String] -> String
combineLines xs = unlines (filter (not . null) xs)
