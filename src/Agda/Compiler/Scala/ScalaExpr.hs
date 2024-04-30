module Agda.Compiler.Scala.ScalaExpr (
 ScalaName,
 ScalaType,
 ScalaExpr(..),
 SeVar(..),
 FunBody,
 unHandled
 ) where

type ScalaName = String
type FunBody   = String     -- this should be some lambda expression
type ScalaType = String

data SeVar = SeVar ScalaName ScalaType
  deriving ( Show )

{- Represent Scala language extracted from internal Agda compiler representation -}
data ScalaExpr
  = SePackage ScalaName [ScalaExpr]
  | SeSum ScalaName [ScalaName]
  | SeFun ScalaName [SeVar] ScalaType FunBody
  | SeProd ScalaName [SeVar]
  | Unhandled ScalaName String
  deriving ( Show )

unHandled :: ScalaExpr -> Bool
unHandled (Unhandled _ _) = True
unHandled _               = False
