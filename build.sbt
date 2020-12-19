name := "elcats"

version := "0.1"

scalaVersion := "2.13.3"
lazy val catsVersion = "2.3.0"

libraryDependencies ++= Seq(
	"org.typelevel" %% "cats-core" % catsVersion
)
