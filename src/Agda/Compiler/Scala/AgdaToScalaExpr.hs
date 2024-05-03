module Agda.Compiler.Scala.AgdaToScalaExpr (
  compileDefn  
  ) where
  
import Agda.Compiler.Backend ( funCompiled, funClauses, Defn(..), RecordData(..))
import Agda.Syntax.Abstract.Name ( QName )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Common ( Arg(..), ArgName, Named(..) )
import Agda.Syntax.Internal (
  Clause(..), DeBruijnPattern, DBPatVar(..), Dom(..), unDom, PatternInfo(..), Pattern'(..),
  qnameName, qnameModule, Telescope, Tele(..), Term(..), Type, Type''(..) )
import Agda.TypeChecking.Monad.Base ( Definition(..) )
import Agda.TypeChecking.Monad
import Agda.TypeChecking.CompiledClause ( CompiledClauses(..), CompiledClauses'(..) )

import Agda.Compiler.Scala.ScalaExpr ( ScalaName, ScalaExpr(..) )

compileDefn :: QName -> Defn -> ScalaExpr
compileDefn defName theDef = case theDef of 
  Datatype{dataCons = dataCons} ->
    compileDataType defName dataCons
  Function{funCompiled = funDef, funClauses = fc} ->
    Unhandled "compileDefn Function" (show defName ++ "\n = \n" ++ show theDef)
  RecordDefn(RecordData{_recFields = recFields, _recTel = recTel}) ->
    Unhandled "compileDefn RecordDefn" (show defName ++ "\n = \n" ++ show theDef)
  other ->
    Unhandled "compileDefn other" (show defName ++ "\n = \n" ++ show theDef)

compileDataType :: QName -> [QName] -> ScalaExpr
compileDataType defName fields = SeAdt (showName defName) (map showName fields)

showName :: QName -> ScalaName
showName = prettyShow . qnameName
