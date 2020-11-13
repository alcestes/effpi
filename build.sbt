val dottyVersion = "3.0.0-M1"
val effpiVersion = "0.0.3"

val useEffpiPlugin = settingKey[Boolean]("Use the effpi compiler plugin in sub-projects.")

inThisBuild(
  // Can be changed from sbt by running `set ThisBuild / useEffpiPlugin := false`
  useEffpiPlugin := true
)

lazy val effpi = (project in file(".")).
  settings(
    name := "effpi",
    version := effpiVersion,

    scalaVersion := dottyVersion,
    //addCompilerPlugin("uk.ac.ic" %% "effpi-verifier" % "0.0.3"),
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
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided",
      ("org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2").withDottyCompat(scalaVersion.value),
      "org.antlr" % "ST4" % "4.3.1"
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
        ("com.typesafe.akka" %% "akka-actor-typed" % "2.6.9").withDottyCompat(scalaVersion.value).
        exclude("org.scala-lang.modules", "scala-java8-compat_2.13")
    ),

    libraryDependencies ++= Seq(
      ("org.scalikejdbc" %% "scalikejdbc" % "3.5.0").withDottyCompat(scalaVersion.value),
      "org.xerial"      % "sqlite-jdbc"      % "3.32.3.2",
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

  Def.taskDyn {
    if (useEffpiPlugin.value) {
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
    else
      Def.task(Seq[String]())
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
    scalacOptions ++= pluginOpts(false, false, true, Some(-1)).value,
  )
