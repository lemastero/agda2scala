{-# LANGUAGE OverloadedStrings #-}

module Agda.Compiler.Scala.PrintScalaExpr ( printScalaExpr
  , printCaseObject
  , printSealedTrait
  , printPackage
  , combineLines
  ) where

import Agda.Compiler.Scala.ScalaExpr ( ScalaName, ScalaExpr(..), SeElem(..), SeArgument )

printScalaExpr :: ScalaExpr -> String
printScalaExpr def = case def of
  (SePackage pName defs) ->
    (printPackage pName) <> defsSeparator
    <> (
      blankLine -- between package declaration and first definition
      <> combineLines (map printScalaExpr defs)
      )
      <> blankLine -- EOF
  (SeAdt adtName adtCases) ->
    (printSealedTrait adtName)
    <> defsSeparator
    <> combineLines (map (printCaseObject adtName) adtCases)
    <> defsSeparator
  (SeFun fName args resType funBody) ->
    "def" <> exprSeparator <> fName
    <> "(" <> combineLines (map printFunArg args) <> ")"
    <> ":" <> exprSeparator <> resType <> exprSeparator
    <> "=" <> exprSeparator <> funBody
    <> defsSeparator
  (Unhandled "" payload) -> ""
  (Unhandled name payload) -> "TODO " ++ (show name) ++ " " ++ (show payload)
  other -> "unsupported printScalaExpr " ++ (show other)

printFunArg :: SeArgument -> String
printFunArg (SeElem sName sType) = sName <> ":" <> exprSeparator <> sType

printSealedTrait :: ScalaName -> String
printSealedTrait adtName = "sealed trait" <> exprSeparator <> adtName

printCaseObject :: ScalaName -> ScalaName -> String
printCaseObject superName caseName =
  "case object" <> exprSeparator <> caseName <> exprSeparator <> "extends" <> exprSeparator <> superName

printPackage :: ScalaName -> String
printPackage pName = "package" <> exprSeparator <> pName

bracket :: String -> String
bracket str = "{\n" <> str <> "\n}"

defsSeparator :: String
defsSeparator = "\n"

blankLine :: String
blankLine = "\n"

exprSeparator :: String
exprSeparator = " "

strip :: String -> String
strip xs = (reverse $ dropWhile (== '\n') (reverse xs)) 

combineLines :: [String] -> String
combineLines xs = strip $ unlines (filter (not . null) xs)
