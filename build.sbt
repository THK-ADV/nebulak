name := "nebulak"
organization := "de.th-koeln.inf.adv"
version := "0.13"
scalaVersion := "3.3.3"

resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

publishTo := Some(
  "GitHub <THK-ADV> Apache Maven Packages" at "https://maven.pkg.github.com/THK-ADV/nebulak"
)

publishConfiguration := publishConfiguration.value.withOverwrite(true)

publishMavenStyle := true

credentials += Credentials(
  "GitHub Package Registry",
  "maven.pkg.github.com",
  "THK-ADV",
  ""
)