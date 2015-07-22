/*
name := "scalaLearn"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases"

resolvers += Classpaths.sbtPluginReleases

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.0.4")

addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.0.0")

import scoverage.coveralls.Imports.CoverallsKeys._
import scoverage.ScoverageSbtPlugin.ScoverageKeys.coverage

coverallsToken := Some("scalaLearn")

libraryDependencies  ++= Seq(
  // other dependencies here
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "com.typesafe.akka" %% "akka-actor" % "2.3.12",
  "org.scalanlp" %% "breeze" % "0.11.2",
  // native libraries are not included by default. add this if you want them (as of 0.7)
  // native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "0.11.2",
  // the visualization library is distributed separately as well.
  // It depends on LGPL code.
  "org.scalanlp" %% "breeze-viz" % "0.11.2"
)

resolvers ++= Seq(
  // other resolvers here
  // if you want to use snapshot builds (currently 0.12-SNAPSHOT), use this.
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)
*/

import com.typesafe.sbt.site.JekyllSupport
import sbt.Keys._
import sbt._
import bintray.Plugin.bintraySettings

import scala.io.Source

import scoverage.ScoverageSbtPlugin.ScoverageKeys.coverage

import com.typesafe.sbt.SbtSite.SiteKeys._

name := "scalaLearn"

lazy val root = project.in(file(".")).
  aggregate(scalaLearnJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val sharedSettings = Seq(
  scalaVersion := "2.11.7",
  version := "0.1.4-SNAPSHOT",
  licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
)

def resourceGenerator(folder: String, sourceType: String, outputPackage: Seq[String]) = {
  baseDirectory map { dir =>
    val fileToWrite = dir / ".." / "shared" / "src" / folder / "scala" / outputPackage.mkString("/") / "Resources.scala"
    val folderToRead = dir / ".." / "shared" / "src" / sourceType / "resources"

    def sourceForDir(directory: File): String = {
      directory.listFiles().map { file =>
        if (file.isDirectory) {
          s"""object ${file.name} {
              |${sourceForDir(file)}
              |}""".stripMargin
        } else {
          val fileLines = Source.fromFile(file).getLines().toList
          val stringList = fileLines.map(s => '"' + s + '"').toString()
          s"""val ${file.name.split('.').head} = $stringList"""
        }
      }.mkString("\n")
    }

    val toWrite =
      s"""package ${outputPackage.mkString(".")}
         |object Resources {
         |${sourceForDir(folderToRead)}
         |}""".stripMargin
    IO.write(fileToWrite, toWrite)
    Seq(fileToWrite)
  }
}

lazy val scalaLearnSettings = sharedSettings ++ Seq(
  organization := "me.ssarangi",
  name := "scalaLearn",
  libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.0" % Test,
  testFrameworks += new TestFramework("utest.runner.Framework"),
  sourceGenerators in Compile <+= resourceGenerator("gen", "main", Seq("me", "ssarangi", "scalaLearn")),
  cleanFiles <+= baseDirectory { base => base / ".." / "shared" / "src" / "gen" },
  sourceGenerators in Test <+= resourceGenerator("testGen", "test", Seq("me", "ssarangi", "scalaLearn", "tests")),
  cleanFiles <+= baseDirectory { base => base / ".." / "shared" / "src" / "testGen" }
) ++ bintraySettings ++ bintrayPublishSettings

lazy val scalaLearn = crossProject.in(file(".")).
  settings(scalaLearnSettings: _*).
  jvmSettings(
    libraryDependencies += "net.databinder.dispatch" %% "dispatch-core" % "0.11.2",
    libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.10" % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.6" % Test
  ).
  jsSettings(
    jsDependencies += RuntimeDOM,
    jsDependencies += ProvidedJS / "bio-pv.min.js" % Test,
    preLinkJSEnv := PhantomJSEnv().value,
    postLinkJSEnv := PhantomJSEnv().value,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.0"
  )

lazy val scalaLearnJVM = scalaLearn.jvm
lazy val scalaLearnJS = scalaLearn.js

lazy val demosSettings = sharedSettings ++ Seq(persistLauncher in Compile := true)

lazy val demos = project.settings(demosSettings: _*).enablePlugins(ScalaJSPlugin).dependsOn(scalaLearn.js)

coverage := {
  coverage.value
  def fileToMkdir = {
    val pathForRoot = new File("").toPath.toAbsolutePath.toString
    s"$pathForRoot/jvm/target/scala-2.11/scoverage-report$pathForRoot/jvm"
  }
  new File(fileToMkdir).mkdirs
}
