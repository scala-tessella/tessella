import sbtcrossproject.CrossProject
import xerial.sbt.Sonatype.*

enablePlugins(SitePreviewPlugin, ParadoxSitePlugin)
enablePlugins(SiteScaladocPlugin)
enablePlugins(GhpagesPlugin)

ThisBuild / organization := "io.github.scala-tessella"
ThisBuild / scalaVersion := "3.3.6"
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / licenses := Seq("APL2" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / publishTo := sonatypePublishToBundle.value
ThisBuild / developers := List(Developer("scala-tessella", "scala-tessella", "mario.callisto@gmail.com", url("https://github.com/scala-tessella")))
ThisBuild / homepage := Some(url("https://github.com/scala-tessella/tessella"))
ThisBuild / scmInfo := Some(ScmInfo(
  url("https://github.com/scala-tessella/tessella"),
  "scm:git:https://github.com/scala-tessella/tessella.git",
  "scm:git:git@github.com:scala-tessella/tessella.git"
))

lazy val root: Project =
  project
    .in(file("."))
    .aggregate(tessella.js, tessella.jvm)
    .settings(
      sonatypeProjectHosting := Some(GitHubHosting("scala-tessella", "tessella", "mario.callisto@gmail.com")),
      sonatypeCredentialHost := "s01.oss.sonatype.org",
      SiteScaladoc / siteSubdirName := "api",
      paradoxProperties += ("scaladoc.base_url" -> "api"),
      libraryDependencies ++= Seq(
        "io.github.scala-tessella" %% "ring-seq" % "0.6.2",
        "io.github.iltotore" %% "iron" % "2.6.0",
        "org.scala-lang.modules" %% "scala-xml" % "2.3.0",
      ),
      git.remoteRepo := sonatypeProjectHosting.value.get.scmUrl,
      ghpagesNoJekyll := true,
      scalacOptions += "-deprecation"
    )

lazy val tessella: CrossProject =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("."))
    .settings(
      name := "tessella",
      description := "Tilings by regular polygons",
      libraryDependencies ++= Seq(
        "io.github.scala-tessella" %%% "ring-seq" % "0.6.2",
        "io.github.iltotore" %%% "iron" % "2.6.0",
        "org.scala-lang.modules" %%% "scala-xml" % "2.3.0",
        "org.scalatest" %%% "scalatest" % "3.2.19" % Test,
        "org.scalacheck" %%% "scalacheck" % "1.18.1" % Test,
        "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test,
      )
    )
    .jvmSettings(
      // Add JVM-specific settings here
    )
    .jsSettings(
      // Add JS-specific settings here
    )

lazy val bench: Project =
  project
    .dependsOn(tessella.jvm % "test->test")
    .enablePlugins(JmhPlugin)
    .settings(
      Jmh / sourceDirectory := (Test / sourceDirectory).value,
      Jmh / classDirectory := (Test / classDirectory).value,
      Jmh / dependencyClasspath := (Test / dependencyClasspath).value,
      // rewire tasks, so that 'bench/Jmh/run' automatically invokes 'bench/Jmh/compile' (otherwise a clean 'bench/Jmh/run' would fail)
      Jmh / compile := (Jmh / compile).dependsOn(Test / compile).value,
      Jmh / run := (Jmh / run).dependsOn(Jmh / compile).evaluated
    )