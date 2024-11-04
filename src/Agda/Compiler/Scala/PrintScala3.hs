module Agda.Compiler.Scala.PrintScala3 ( printScala3
  , printCaseObject
  , printSealedTrait
  , printPackageAndObject
  , printCaseClass
  , combineLines
  ) where

import Data.List ( intercalate )
import Agda.Compiler.Scala.ScalaExpr ( ScalaName, ScalaExpr(..), SeVar(..))

printScala3 :: ScalaExpr -> String
printScala3 def = case def of
  (SePackage pNames defs) ->
    (printPackageAndObject pNames) <> exprSeparator
      <> bracket (
      blankLine -- between package declaration and first definition
      <> combineLines (map printScala3 defs)
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
  other -> "unsupported printScala3 " ++ (show other)

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

printPackageAndObject :: [ScalaName] -> String
printPackageAndObject [] = ""
printPackageAndObject (oname:[]) = printObject oname
printPackageAndObject pName = printPackage (init pName)
  <> defsSeparator <> defsSeparator
  <> (printObject (last pName))
  
printPackage :: [ScalaName] -> String
printPackage [] = ""
printPackage pNames = "package" <> exprSeparator <> (intercalate "." pNames)

printObject :: ScalaName -> String
printObject pName = "object" <> exprSeparator <> pName

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
