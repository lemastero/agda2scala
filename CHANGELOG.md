# Revision history for agda2scala

## 0.1.0.1

* Save compiled Scala code in file
* Compile Agda module into Scala package
* Generate ADT with only case objects
* CI: compile example Agda code

## 0.1.0.2

* Compile data with ADT into Scala case classes and case objects
* Remove excessive new lines in output Scala file
* CI: compile output Scala code using SBT
* Refactor: split Scala expressions, printing Scala expressions, compile internal Agda representation to Scala expressions

## 0.1.0.3
* add flag `--scala-dialect=Scala3` and support output for Scala3
* test generated Scala 2 and Scala 3 code in CI build
