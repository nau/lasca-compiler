name := "newlang"

organization := "com.newniverse"

version := "0.1-SNAPSHOT"

//scalaVersion := "2.11.8"

enablePlugins(DottyPlugin)

antlr4Settings

antlr4GenVisitor in Antlr4 := true

antlr4PackageName in Antlr4 := Some("com.newniverse.parser")