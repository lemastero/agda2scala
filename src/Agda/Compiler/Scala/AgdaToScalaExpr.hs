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

import Agda.Compiler.Scala.ScalaExpr ( ScalaName, ScalaType, FunBody, ScalaExpr(..), SeElem(..) )

compileDefn :: QName -> Defn -> ScalaExpr
compileDefn defName theDef = case theDef of 
  Datatype{dataCons = dataCons} ->
    compileDataType defName dataCons
  Function{funCompiled = funDef, funClauses = fc} ->
    compileFunction defName funDef fc
  RecordDefn(RecordData{_recFields = recFields, _recTel = recTel}) ->
    Unhandled "compileDefn RecordDefn" (show defName ++ "\n = \n" ++ show theDef)
  other ->
    Unhandled "compileDefn other" (show defName ++ "\n = \n" ++ show theDef)

compileDataType :: QName -> [QName] -> ScalaExpr
compileDataType defName fields = SeAdt (fromQName defName) (map fromQName fields)

compileFunction :: QName
  -> Maybe CompiledClauses
  -> [Clause]
  -> ScalaExpr
compileFunction defName funDef fc =
  SeFun
    (fromQName defName)
    [SeElem (compileFunctionArgument fc) (compileFunctionArgType fc)] -- TODO many function arguments
    (compileFunctionResultType fc)
    (compileFunctionBody funDef)

compileFunctionArgument :: [Clause] -> ScalaName
compileFunctionArgument [] = ""
compileFunctionArgument [fc] = fromDeBruijnPattern (namedThing (unArg (head (namedClausePats fc))))
compileFunctionArgument xs = error "unsupported compileFunctionArgument" ++ (show xs) -- show xs

compileFunctionArgType :: [Clause] -> ScalaType
compileFunctionArgType [ Clause{clauseTel = ct} ] = fromTelescope ct
compileFunctionArgType xs = error "unsupported compileFunctionArgType" ++ (show xs)

fromTelescope :: Telescope -> ScalaName
fromTelescope tel = case tel of
  ExtendTel a _ -> fromDom a
  other -> error ("unhandled fromType" ++ show other)

fromDom :: Dom Type -> ScalaName
fromDom x = fromType (unDom x)

compileFunctionResultType :: [Clause] -> ScalaType
compileFunctionResultType [Clause{clauseType = ct}] = fromMaybeType ct
compileFunctionResultType other = error ("unhandled compileFunctionResultType" ++ show other)

fromMaybeType :: Maybe (Arg Type) -> ScalaName
fromMaybeType (Just argType) = fromArgType argType
fromMaybeType other = error ("unhandled fromMaybeType" ++ show other)

fromArgType :: Arg Type -> ScalaName
fromArgType arg = fromType (unArg arg)

fromType :: Type -> ScalaName
fromType t = case t of
  a@(El _ ue) -> fromTerm ue
  other -> error ("unhandled fromType" ++ show other)

fromTerm :: Term -> ScalaName
fromTerm t = case t of
  Def qname el -> fromQName qname
  other -> error ("unhandled fromTerm" ++ show other)

fromDeBruijnPattern :: DeBruijnPattern -> ScalaName
fromDeBruijnPattern d = case d of
    VarP a b -> (dbPatVarName b)
    a@(ConP x y z) -> show a
    other -> error ("unhandled fromDeBruijnPattern" ++ show other)

compileFunctionBody :: Maybe CompiledClauses -> FunBody
compileFunctionBody (Just funDef) = fromCompiledClauses funDef
compileFunctionBody funDef = error ("unhandled compileFunctionBody " ++ show funDef)

fromCompiledClauses :: CompiledClauses -> FunBody
fromCompiledClauses cc = case cc of
  (Done (x:xs) term) -> fromArgName x
  other               -> error ("unhandled fromCompiledClauses " ++ show other)

fromArgName :: Arg ArgName -> FunBody
fromArgName = unArg

fromQName :: QName -> ScalaName
fromQName = prettyShow . qnameName
