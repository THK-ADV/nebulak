name := "nebulak"
organization := "de.th-koeln.inf.adv"
version := "0.14"
scalaVersion := "3.3.3"

homepage := Some(url("https://github.com/THK-ADV/nebulak"))
licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT"))
scmInfo := Some(
  ScmInfo(url("https://github.com/THK-ADV/nebulak"), "scm:git@github.com:THK-ADV/nebulak.git")
)

resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

publishTo := Some(
  "GitHub <THK-ADV> Apache Maven Packages" at "https://maven.pkg.github.com/THK-ADV/nebulak"
)

publishConfiguration := publishConfiguration.value.withOverwrite(true)

publishMavenStyle := true

credentials ++= {
  def loadEnvFile(f: java.io.File): Map[String, String] =
    if (f.exists()) {
      val lines = scala.io.Source.fromFile(f).getLines()
      lines
        .filter(line => line.contains("=") && !line.trim.startsWith("#"))
        .map { line =>
          val i = line.indexOf('=')
          val k = line.take(i).trim
          val v = line.drop(i + 1).trim
          val v2 = if (v.startsWith("\"") && v.endsWith("\"")) v.drop(1).dropRight(1) else v
          (k, v2)
        }
        .toMap
    } else Map.empty

  val envFile = file(".env")
  val envMap  = loadEnvFile(envFile)
  def get(key: String): String =
    sys.env.getOrElse(key, envMap.getOrElse(key, "")).trim
  val user  = get("GITHUB_PACKAGES_USER")
  val token = Option(get("GITHUB_PACKAGES_TOKEN")).filter(_.nonEmpty).orElse(Option(get("GITHUB_TOKEN")).filter(_.nonEmpty))

  if (token.nonEmpty)
    Seq(Credentials("GitHub Package Registry", "maven.pkg.github.com", if (user.nonEmpty) user else "THK-ADV", token.get))
  else
    Seq.empty
}