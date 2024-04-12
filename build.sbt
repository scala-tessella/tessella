import xerial.sbt.Sonatype._

val scalatest = "org.scalatest" %% "scalatest" % "3.2.17" % "test"
val scalacheck = "org.scalacheck" %% "scalacheck" % "1.17.0" % "test"

enablePlugins(SitePreviewPlugin, ParadoxSitePlugin)
enablePlugins(SiteScaladocPlugin)

ThisBuild / organization := "io.github.scala-tessella"
ThisBuild / scalaVersion := "3.3.3"
ThisBuild / semanticdbEnabled := true

lazy val root = (project in file("."))
  .settings(
    name := "tessella",
    description := "Tessellations by regular polygons",
    version := "0.1.1",
    licenses := Seq("APL2" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    description := "Tilings by regular polygons",
    sonatypeProjectHosting := Some(GitHubHosting("scala-tessella", "tessella", "mario.callisto@gmail.com")),
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    publishTo := sonatypePublishToBundle.value,
    git.remoteRepo := sonatypeProjectHosting.value.get.scmUrl,
    ghpagesNoJekyll := true,
    libraryDependencies ++= Seq(
      "io.github.iltotore" %% "iron" % "2.5.0",
      "math.geom2d" % "javaGeom" % "0.11.1",
      "org.scala-lang.modules" %% "scala-xml" % "2.2.0",
      "io.github.scala-tessella" %% "ring-seq" % "0.4.0",
      scalatest,
      scalacheck
    ),
    SiteScaladoc / siteSubdirName := "api",
    paradoxProperties += ("scaladoc.base_url" -> "api"),
    coverageEnabled := true,
    scalacOptions += "-deprecation"
  )