module Agda.Compiler.Scala.AgdaToScalaExpr ( compileDefn ) where
  
import Agda.Compiler.Backend ( funCompiled, funClauses, Defn(..), RecordData(..))
import Agda.Syntax.Abstract.Name ( QName )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Common ( Arg(..), ArgName, Named(..), NamedName, WithOrigin(..), Ranged(..) )
import Agda.Syntax.Internal (
  Clause(..), DeBruijnPattern, DBPatVar(..), Dom(..), Dom'(..), unDom, PatternInfo(..), Pattern'(..),
  qnameName, qnameModule, Telescope, Tele(..), Term(..), Type, Type''(..) )
import Agda.TypeChecking.Monad.Base ( Definition(..) )
import Agda.TypeChecking.Monad
import Agda.TypeChecking.CompiledClause ( CompiledClauses(..), CompiledClauses'(..) )
import Agda.TypeChecking.Telescope ( teleNamedArgs, teleArgs, teleArgNames )

import Agda.Syntax.Common.Pretty ( prettyShow )

import Agda.Compiler.Scala.ScalaExpr ( ScalaName, ScalaType, FunBody, ScalaExpr(..), SeVar(..) )

compileDefn :: QName -> Defn -> ScalaExpr
compileDefn defName theDef = case theDef of 
  Datatype{dataCons = dataCons} ->
    compileDataType defName dataCons
  Function{funCompiled = funCompiled, funClauses = funClauses} ->
    compileFunction defName funCompiled funClauses
  RecordDefn(RecordData{_recFields = recFields, _recTel = recTel}) ->
    compileRecord defName recFields recTel
  other ->
    Unhandled "compileDefn other" (show defName ++ "\n = \n" ++ show theDef)

compileRecord :: QName -> [Dom QName] -> Telescope -> ScalaExpr
compileRecord defName recFields recTel = SeProd (fromQName defName) (foldl varsFromTelescope [] recTel)

varsFromTelescope :: [SeVar] -> Dom Type -> [SeVar]
varsFromTelescope xs dt = SeVar (nameFromDom dt) (fromDom dt) : xs

compileDataType :: QName -> [QName] -> ScalaExpr
compileDataType defName fields = SeSum (fromQName defName) (map fromQName fields)

compileFunction :: QName
  -> Maybe CompiledClauses
  -> [Clause]
  -> ScalaExpr
compileFunction defName funCompiled funClauses =
  SeFun
    (fromQName defName) -- ++ "\n FULL FUNCTION DEFINITION \n[\n" ++ (show theDef) ++ "\n]\n")
    (funArgs funClauses)
    (compileFunctionResultType funClauses)
    -- you can get body of the function using:
    -- - FunctionData _funCompiled
    -- - FunctionData _funClauses Clause clauseBody
    -- see:
    -- https://hackage.haskell.org/package/Agda-2.6.4.3/docs/Agda-TypeChecking-Monad-Base.html#t:FunctionData
    -- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Internal.html#t:Clause
    -- at this point both contain the same info (at least in simple cases)
    (compileFunctionBody funCompiled)

funArgs :: [Clause] -> [SeVar]
funArgs [] = []
funArgs (c : cs) = funArgsFromClause c

funArgsFromClause :: Clause -> [SeVar]
funArgsFromClause c@Clause{clauseTel = clauseTel} = case parsedArgs of
    [(SeVar "" varType)] -> [SeVar (hackyFunArgNameFromClause c) varType]
    args                 -> args
  where
    parsedArgs = foldl varsFromTelescope [] clauseTel

-- this is extremely hacky way to get function argument name
-- for identity function
-- I apparently do not understand enough how this works
-- or perhaps this is bug in Agda compiler :)
hackyFunArgNameFromClause :: Clause -> ScalaName
hackyFunArgNameFromClause fc = hackyFunArgNameFromDeBruijnPattern (namedThing (unArg
  (head         -- TODO perhaps iterate here
    (namedClausePats fc))))

hackyFunArgNameFromDeBruijnPattern :: DeBruijnPattern -> ScalaName
hackyFunArgNameFromDeBruijnPattern d = case d of
    VarP a b -> (dbPatVarName b)
    a@(ConP x y z) -> "\n hackyFunArgNameFromDeBruijnPattern \n[\n" ++ show a ++ "\n]\n" 
    other -> error ("hackyFunArgNameFromDeBruijnPattern " ++ show other)

nameFromDom :: Dom Type -> ScalaName
nameFromDom dt = case (domName dt) of
  Nothing -> ""
  Just a -> namedNameToStr a

-- https://hackage.haskell.org/package/Agda-2.6.4.3/docs/Agda-Syntax-Common.html#t:NamedName
namedNameToStr :: NamedName -> ScalaName
namedNameToStr n = rangedThing (woThing n)

fromDom :: Dom Type -> ScalaName
fromDom x = fromType (unDom x)

compileFunctionResultType :: [Clause] -> ScalaType
compileFunctionResultType [Clause{clauseType = ct}] = fromMaybeType ct
compileFunctionResultType (Clause{clauseType = ct} : xs) = fromMaybeType ct
compileFunctionResultType other = error "Fatal error - function has not clause."

fromMaybeType :: Maybe (Arg Type) -> ScalaName
fromMaybeType (Just argType) = fromArgType argType
fromMaybeType other = error ("\nunhandled fromMaybeType \n[" ++ show other ++ "]\n")

fromArgType :: Arg Type -> ScalaName
fromArgType arg = fromType (unArg arg)

fromType :: Type -> ScalaName
fromType t = case t of
  El _ ue -> fromTerm ue
  other -> error ("unhandled fromType [" ++ show other ++ "]")

-- https://hackage.haskell.org/package/Agda-2.6.4.3/docs/Agda-Syntax-Internal.html#t:Term
fromTerm :: Term -> ScalaName
fromTerm t = case t of
  Def qName elims -> fromQName qName
  Var n elims -> "\nunhandled fromTerm Var \n[" ++ show t ++ "]\n"
  other -> error ("\nunhandled fromTerm [" ++ show other ++ "]\n")

compileFunctionBody :: Maybe CompiledClauses -> FunBody
compileFunctionBody (Just funDef) = fromCompiledClauses funDef
compileFunctionBody funDef = error "Fatal error - function body is not compiled."

-- https://hackage.haskell.org/package/Agda/docs/Agda-TypeChecking-CompiledClause.html#t:CompiledClauses
fromCompiledClauses :: CompiledClauses -> FunBody
fromCompiledClauses cc = case cc of
  (Case argInt caseCompiledClauseTerm) -> "WIP" --"\nCase fromCompiledClauses\n[\n" ++ (show cc) ++ "\n]\n"
  (Done (x:xs) term) -> fromArgName x
  other               -> "\nunhandled fromCompiledClauses \n\n[" ++ show other ++ "]\n"

fromArgName :: Arg ArgName -> FunBody
fromArgName = unArg

fromQName :: QName -> ScalaName
fromQName = prettyShow . qnameName
