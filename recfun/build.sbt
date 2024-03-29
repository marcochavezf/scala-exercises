course := "progfun1"
assignment := "recfun"

// scalaVersion := "2.13.1"
scalaVersion := "0.22.0-RC1"

scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")
