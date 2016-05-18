name := "funpats"
version := "0.0.1-SNAPSHOT"
scalaVersion := "2.11.8"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.1.0")

libraryDependencies ++= Seq(
  "com.github.mpilquist" %% "simulacrum" % "0.7.0",
  "org.scodec" %% "scodec-bits" % "1.1.0"
)

scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-language:higherKinds"
)

initialCommands in console := """import funpats._"""