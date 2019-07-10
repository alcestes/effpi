val dottyVersion = "0.9.0"
//val dottyVersion = "0.11.0-RC1"
val effpiVersion = "0.0.2"

lazy val effpi = (project in file(".")).
  settings(
    name := "effpi",
    version := effpiVersion,

    scalaVersion := dottyVersion,
    //addCompilerPlugin("uk.ac.ic" %% "effpi-verifier" % "0.0.1"),
  )

lazy val demo = project
  .in(file("demo"))
  .dependsOn(effpi)
  //.dependsOn(plugin)
  .settings(
    name := "effpi-demo",
    version := effpiVersion,
    scalaVersion := dottyVersion,
  )

lazy val plugin = project
  .in(file("plugin"))
  .dependsOn(effpi)
  .settings(
    name := "effpi-verifier",
    version := effpiVersion,
    scalaVersion := dottyVersion,

    // TODO: exclude unnecessary dependencies from assembly jar
    libraryDependencies ++= Seq(
      "ch.epfl.lamp" %% "dotty-compiler" % scalaVersion.value % "provided",
      "org.scala-lang.modules" % "scala-parser-combinators_2.12" % "1.1.1",
      "org.clapper" % "scalasti_2.12" % "3.0.1"
    ),
  )

lazy val benchmarks = project
  .in(file("benchmarks"))
  .dependsOn(effpi)
  .settings(
    name := "effpi-benchmarks",
    version := effpiVersion,
    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
        ("com.typesafe.akka" % "akka-actor-typed_2.12" % "2.5.17").
        exclude("org.scala-lang.modules", "scala-java8-compat_2.12")
    ),

    libraryDependencies ++= Seq(
      "org.scalikejdbc" % "scalikejdbc_2.12" % "3.3.1",
      "org.xerial"      % "sqlite-jdbc"      % "3.23.1",
      "ch.qos.logback"  % "logback-classic"  % "1.2.3"
    ),

    mainClass in assembly := Some("effpi.benchmarks.main.Main"),
  )

// Options for loading the compiler plugin
//
// keepTmp: if true, preserve temporary files generated during verification
//
// skipLts: if true, never invoke `lps2lts` (saves time & memory)
//
// benchmarkOverride: allows to enforce a fixed number of benchmark repetitions,
// useful for testing.  Possible values are:
//   * None     means "no override": obey benchmark options in code annotations
//   * Some(-1) means "no verification, just type checking and sanity checks"
//   * Some(0)  means "no benchmark repetitions, just verification"
//   * Some(n)  (with n > 0) means "always perform n benchmark repetitions"
def pluginOpts(verbose: Boolean, keepTmp: Boolean,
               skipLts: Boolean = false,
               benchmarkOverride: Option[Int] = None) = {
  val pluginName = "effpiVerifier"
  
  // Uncomment (and comment line below) to change the task providing `jar`.
  // NOTE: since the plugin has external dependencies, we need a fat jar
  // (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
  (`assemblyOutputPath` in (plugin, assembly)) map { (jar: File) =>
    val addPlugin = s"-Xplugin:${jar.getAbsolutePath}"

    // Option to enable verbose logging
    val loggingOpt = s"-Ylog:${pluginName}+"
    
    // Option for *not* removing temporary files
    val keepTmpOpt = "keep-tmp"

    // Option for overriding benchmark repetitions
    val benchOverrideOpt = "bench-override"

    // Option for never generating LTSs:
    val skipLtsOpt = "skip-lts"
    
    // Hack taken from https://github.com/retronym/boxer
    // Adding the plugin timestamp to compiler options triggers recompilation
    // after editing the plugin (otherwise, a manual 'clean' is needed).
    // NOTE: this dependency works with Compile, but not with assembly :-\
    val dummyOpt = s"timestamp=${jar.lastModified}"
    
    val opts = {
      Seq(s"${pluginName}:${dummyOpt}") ++
      (if (keepTmp) Seq(s"${pluginName}:${keepTmpOpt}") else Seq()) ++
      (if (skipLts) Seq(s"${pluginName}:${skipLtsOpt}") else Seq()) ++
      (benchmarkOverride match {
        case None => Seq()
        case Some(n) => {
          assert((n >= 0) || (n == -1))
          Seq(s"${pluginName}:${benchOverrideOpt}=${n}")
        }
      })
    }.mkString(",")

    Seq(addPlugin) ++ Seq(s"-P:${opts}") ++ {
      if (verbose) Seq(loggingOpt) else Seq()
    }
  }
}

lazy val examples = project
  .in(file("examples"))
  .dependsOn(effpi)
  .settings(
    name := "effpi-examples",
    version := effpiVersion,
    scalaVersion := dottyVersion,
    scalacOptions ++= pluginOpts(false, false).value,
  )

lazy val pluginBenchmarks = project
  .in(file("plugin-benchmarks"))
  .dependsOn(effpi)
  .settings(
    name := "effpi-plugin-benchmarks",
    version := effpiVersion,
    scalaVersion := dottyVersion,
    scalacOptions ++= pluginOpts(false, false, true, Some(2)).value,
  )
