module Agda.Compiler.Scala.ScalaExpr (
 ScalaName,
 ScalaType,
 SeArgument,
 ScalaExpr(..),
 SeElem(..),
 FunBody,
 unHandled
 ) where

type ScalaName = String
type FunBody = String     -- this should be some lambda expression
type ScalaType = String

data SeElem = SeElem ScalaName ScalaType
  deriving ( Show )
  
type SeArgument = SeElem

{- Represent Scala language extracted from Agda compiler representation -}
data ScalaExpr
  = SePackage ScalaName [ScalaExpr]
  | SeAdt ScalaName [ScalaName]
  | SeFun ScalaName [SeArgument] ScalaType FunBody
  | Unhandled ScalaName String
  deriving ( Show )

unHandled :: ScalaExpr -> Bool
unHandled (Unhandled _ _) = True
unHandled _               = False
