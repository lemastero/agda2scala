module Agda.Compiler.Scala.Expr (
 ScalaName,
 ScalaExpr(..),
 unHandled
 ) where

type ScalaName = String

{- Represent Scala language extracted from Agda compiler representation -}
data ScalaExpr
  = SePackage ScalaName [ScalaExpr]
  | SeAdt ScalaName [ScalaName]
  | Unhandled ScalaName String
  deriving ( Show )

unHandled :: ScalaExpr -> Bool
unHandled (Unhandled _ _) = True
unHandled _               = False
