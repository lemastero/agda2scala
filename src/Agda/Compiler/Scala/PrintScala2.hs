module Agda.Compiler.Scala.PrintScala2 ( printScala2
  , printCaseObject
  , printSealedTrait
  , printPackage
  , printCaseClass
  , combineLines
  ) where

import Data.List ( intercalate )
import Agda.Compiler.Scala.ScalaExpr ( ScalaName, ScalaExpr(..), SeVar(..))

printScala2 :: ScalaExpr -> String
printScala2 def = case def of
  (SePackage pName defs) ->
    (printPackage pName) <> exprSeparator -- TODO this should be package + object
      <> bracket (
      blankLine -- between package declaration and first definition
      <> combineLines (map printScala2 defs)
      )
      <> blankLine -- EOF
  (SeSum adtName adtCases) ->
    (printSealedTrait adtName)
    <> defsSeparator
    <> combineLines (map (printCaseObject adtName) adtCases)
    <> defsSeparator
  (SeFun fName args resType funBody) ->
    "def" <> exprSeparator <> fName
    <> "(" <> combineThem (map printVar args) <> ")"
    <> ":" <> exprSeparator <> resType <> exprSeparator
    <> "=" <> exprSeparator <> funBody
    <> defsSeparator
  (SeProd name args) -> printCaseClass name args <> defsSeparator
  (Unhandled "" payload) -> ""
  (Unhandled name payload) -> "TODO " ++ (show name) ++ " " ++ (show payload)
  other -> "unsupported printScala2 " ++ (show other)

printCaseClass :: ScalaName -> [SeVar] -> String
printCaseClass name args = "final case class" <> exprSeparator <> name <> "(" <> (printExpr args) <> ")"

printVar :: SeVar -> String
printVar (SeVar sName sType) = sName <> ":" <> exprSeparator <> sType

printExpr :: [SeVar] -> String
printExpr names = combineThem (map printVar names)

combineThem :: [String] -> String
combineThem xs = intercalate ", " xs

printSealedTrait :: ScalaName -> String
printSealedTrait adtName = "sealed trait" <> exprSeparator <> adtName

printCaseObject :: ScalaName -> ScalaName -> String
printCaseObject superName caseName =
  "case object" <> exprSeparator <> caseName <> exprSeparator <> "extends" <> exprSeparator <> superName

printPackage :: ScalaName -> String
printPackage pName = "object" <> exprSeparator <> pName

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
