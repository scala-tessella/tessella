import xerial.sbt.Sonatype.*

enablePlugins(SitePreviewPlugin, ParadoxSitePlugin)
enablePlugins(SiteScaladocPlugin)
enablePlugins(GhpagesPlugin)

ThisBuild / organization := "io.github.scala-tessella"
ThisBuild / scalaVersion := "3.3.3"
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

lazy val root: Project =
  project
    .in(file("."))
    .settings(
      name := "tessella",
      description := "Tilings by regular polygons",
      licenses := Seq("APL2" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
      sonatypeProjectHosting := Some(GitHubHosting("scala-tessella", "tessella", "mario.callisto@gmail.com")),
      sonatypeCredentialHost := "s01.oss.sonatype.org",
      publishTo := sonatypePublishToBundle.value,
      git.remoteRepo := sonatypeProjectHosting.value.get.scmUrl,
      ghpagesNoJekyll := true,
      libraryDependencies ++= Seq(
        "io.github.iltotore" %% "iron" % "2.5.0",
        "math.geom2d" % "javaGeom" % "0.11.1",
        "org.scala-lang.modules" %% "scala-xml" % "2.3.0",
        "io.github.scala-tessella" %% "ring-seq" % "0.5.1",
        "org.scalatest" %% "scalatest" % "3.2.18" % "test",
        "org.scalacheck" %% "scalacheck" % "1.17.0" % "test"
      ),
      SiteScaladoc / siteSubdirName := "api",
      paradoxProperties += ("scaladoc.base_url" -> "api"),
      scalacOptions += "-deprecation"
    )
