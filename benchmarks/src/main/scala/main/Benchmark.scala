// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.benchmarks.main

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.{ Future, Promise, Await }
import scala.concurrent.ExecutionContext.Implicits.global

import java.io.{BufferedWriter, FileWriter}

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer
import scala.util.Random

import scala.language.implicitConversions

import java.sql.DriverManager
import scalikejdbc._//{ConnectionPool, AutoSession, sql}

import effpi.system.{ProcessSystemStateMachineMultiStep,
                     ProcessSystemRunnerImproved}
import effpi.benchmarks.{effpi => effpib, akka => akkab}

object Main {
  Class.forName("org.sqlite.JDBC") // Import SQLite JDBC driver
  GlobalSettings.loggingConnections = false
  GlobalSettings.loggingSQLAndTime = LoggingSQLAndTimeSettings(
    enabled = false,
    singleLineMode = false,
    printUnprocessedStackTrace = false,
    stackTraceDepth= 15,
    logLevel = Symbol("warn"),
    warningEnabled = false,
    warningThresholdMillis = 3000L,
    warningLogLevel = Symbol("warn")
  )

  // Output file
  val BENCH_OUTFILE = "benchmarks.db"

  // Benchmark types
  val BENCH_SIZE_TIME = "size_vs_time"
  val BENCH_SIZE_MEMORY = "size_vs_memory"

  // Benchmark names, as strings
  val CHAMENEOS = "chameneos"
  val COUNTING = "counting"
  val FORKJOIN_CREATION = "forkjoin_creation"
  val FORKJOIN_THROUGHPUT = "forkjoin_throughput"
  val PINGPONG = "pingpong"
  val RING = "ring"
  val RINGSTREAM = "ringstream"

  // System setups, as strings
  val EFFPI_STATEMACHINE = "statemachinemultistep"
  val EFFPI_RUNNER = "runnerimproved"
  val AKKA = "akka"

  def main(args: Array[String]) = {
    import java.time.{Instant, ZonedDateTime}
    import java.time.format.DateTimeFormatter.{RFC_1123_DATE_TIME => RFC}

    if (args.length != 5) {
      println("missing arguments")
      System.exit(21)
    }
    val benchType = args(0)
    val system = args(1)
    val benchName = args(2)

    val repetitions = args(3).toInt

    val benchGroupId = args(4).toInt

    val startTime = ZonedDateTime.now()
    println(s"Benchmark starting: ${startTime.format(RFC)}")

    val cfg = new org.sqlite.SQLiteConfig()
    cfg.enforceForeignKeys(true)
    val db = DB(DriverManager.getConnection(s"jdbc:sqlite:${BENCH_OUTFILE}",
                                            cfg.toProperties))
    benchType match {
      case BENCH_SIZE_TIME => benchSizeVsTime(benchGroupId, benchName, system, repetitions, db)
      case BENCH_SIZE_MEMORY => benchSizeVsMemory(benchGroupId, benchName, system, db)
      case unsupported =>
        throw new RuntimeException(s"Unsupported benchmark type: ${unsupported}")
    }

    db.close()

    val endTime = ZonedDateTime.now()
    println(s"Benchmark ended: ${endTime.format(RFC)}")
    val seconds = java.time.Duration.between(startTime, endTime).getSeconds
    val (hh, mm, ss) = (seconds / 3600, (seconds / 60) % 60, seconds % 60)
    println("Total time: %d:%02d:%02d".format(hh, mm, ss))
  }

  // Representation of a benchmark
  case class BenchmarkFun[A](stateMachine: A => Long,
                             runner: A => Long,
                             akka: A => Long)
  case class Benchmark[A](params: List[A],
                          sqlInsert: (benchId: Long, params: A, session: DBSession) => Unit,
                          fun: BenchmarkFun[A])

  val sizeBenchmarks = Map(
    CHAMENEOS -> Benchmark(
      List((2,100000),(10,100000),(50,100000),(100,100000),(500,100000),(1500,100000),(5000,100000),(15000,100000),(50000,100000),(150000,100000),(500000,100000),(2500000,100000)),
      { (benchId, p, session) =>
        sql"insert into benchmark_chameneos (`id`, `name`, `size`, `meetings`) values (${benchId}, ${CHAMENEOS}, ${p._1}, ${p._2})".update.apply()(session)
      },
      BenchmarkFun(
        (param: (Int, Int)) => effpib.Chameneos.bench(param, () => ProcessSystemStateMachineMultiStep()),
        (param: (Int, Int)) => effpib.Chameneos.bench(param, () => ProcessSystemRunnerImproved()),
        (param: (Int, Int)) => akkab.Chameneos.bench(param)
      )
    ),
    COUNTING -> Benchmark(
      List(100,250,500,750,1000,10000,100000,1000000,10000000),
      { (benchId, p, session) =>
        sql"insert into benchmark_counting (`id`, `name`, `count`) values (${benchId}, ${COUNTING}, ${p})".update.apply()(session)
      },
      BenchmarkFun(
        (param: Int) => effpib.CountingActor.bench(param, () => ProcessSystemStateMachineMultiStep()),
        (param: Int) => effpib.CountingActor.bench(param, () => ProcessSystemRunnerImproved()),
        (param: Int) => akkab.CountingActor.bench(param)
      )
    ),
    FORKJOIN_CREATION -> Benchmark(
      List(2,10,50,100,500,1500,5000,15000,100000,500000,5000000),
      { (benchId, p, session) =>
        sql"insert into benchmark_forkjoin_creation (`id`, `name`, `size`) values (${benchId}, ${FORKJOIN_CREATION}, ${p})".update.apply()(session)
      },
      BenchmarkFun(
        (param: Int) => effpib.ForkJoinCreation.bench(param, () => ProcessSystemStateMachineMultiStep()),
        (param: Int) => effpib.ForkJoinCreation.bench(param, () => ProcessSystemRunnerImproved()),
        (param: Int) => akkab.ForkJoinCreation.bench(param)
      )
    ),
    FORKJOIN_THROUGHPUT -> Benchmark(
      List((2,500),(10,500),(50,500),(100,500),(500,500),(1500,500),(5000,500),(15000,500),(50000,500),(150000,500)),
      { (benchId, p, session) =>
        sql"insert into benchmark_forkjoin_throughput (`id`, `name`, `size`, `messages`) values (${benchId}, ${FORKJOIN_THROUGHPUT}, ${p._1}, ${p._2})".update.apply()(session)
      },
      BenchmarkFun(
        (param: (Int, Int)) => effpib.ForkJoinThroughput.bench(param, () => ProcessSystemStateMachineMultiStep()),
        (param: (Int, Int)) => effpib.ForkJoinThroughput.bench(param, () => ProcessSystemRunnerImproved()),
        (param: (Int, Int)) => akkab.ForkJoinThroughput.bench(param)
      )
    ),
    PINGPONG -> Benchmark(
      List((2,100),(10,100),(50,100),(100,100),(500,100),(1500,100),(5000,100),(15000,100),(50000,100),(150000,100)),
      { (benchId, p, session) =>
        sql"insert into benchmark_pingpong (`id`, `name`, `pairs`, `exchanges`) values (${benchId}, ${PINGPONG}, ${p._1}, ${p._2})".update.apply()(session)
      },
      BenchmarkFun(
        (param: (Int, Int)) => effpib.PingPong.bench(param, () => ProcessSystemStateMachineMultiStep()),
        (param: (Int, Int)) => effpib.PingPong.bench(param, () => ProcessSystemRunnerImproved()),
        (param: (Int, Int)) => akkab.PingPong.bench(param)
      )
    ),
    RING -> Benchmark(
      List((2,150000,1),(10,150000,1),(50,150000,1),(100,150000,1),(500,150000,1),(1500,150000,1),(5000,150000,1),(15000,150000,1),(50000,150000,1),(150000,150000,1)),
      { (benchId, p, session) =>
        sql"insert into benchmark_ring (`id`, `name`, `size`, `hops`) values (${benchId}, ${RING}, ${p._1}, ${p._2})".update.apply()(session)
      },
      BenchmarkFun(
        (param: (Int, Int, Int)) => effpib.Ring.bench(param, () => ProcessSystemStateMachineMultiStep()),
        (param: (Int, Int, Int)) => effpib.Ring.bench(param, () => ProcessSystemRunnerImproved()),
        (param: (Int, Int, Int)) => akkab.Ring.bench(param)
      )
    ),
    RINGSTREAM -> Benchmark(
      List((2,3000,1000),(10,3000,1000),(50,3000,1000),(100,3000,1000),(500,3000,1000),(1500,3000,1000),
           (5000,3000,1000),(15000,3000,1000), // FIXME: these values do not trigger all actors
           (50000,3000,1000),(150000,3000,1000)),
      { (benchId, p, session) =>
        sql"insert into benchmark_ringstream (`id`, `name`, `size`, `hops`, `messages`) values (${benchId}, ${RINGSTREAM}, ${p._1}, ${p._2}, ${p._3})".update.apply()(session)
      },
      BenchmarkFun(
        (param: (Int, Int, Int)) => effpib.Ring.bench(param, () => ProcessSystemStateMachineMultiStep()),
        (param: (Int, Int, Int)) => effpib.Ring.bench(param, () => ProcessSystemRunnerImproved()),
        (param: (Int, Int, Int)) => akkab.Ring.bench(param)
      )
    )
  )

  def benchSizeVsTime(benchGroupId: Long,
                      benchName: String, system: String, repetitions: Int,
                      db: DB, reduced: Boolean = false) = {
    if (!sizeBenchmarks.keySet.contains(benchName)) {
      throw new RuntimeException(s"Unsupported benchmark: ${benchName}")
    }

    println(s"Running benchmark (size vs. time): ${benchName}; system: ${system}")

    val benchmark = sizeBenchmarks(benchName)
    val params = if (reduced) benchmark.params.take(2) else benchmark.params

    db.autoCommit { implicit session =>
      params.foreach { p =>
        val benchId = sql"insert into benchmark(`group`, `type`, `name`, `system`, `start`) values (${benchGroupId}, ${BENCH_SIZE_TIME}, ${benchName}, ${system}, ${System.currentTimeMillis})".updateAndReturnGeneratedKey.apply()
        benchmark.sqlInsert(benchId, p, session)
        (1 to repetitions).foreach { r =>
          System.gc()
          val nanosecs = system match {
            case EFFPI_STATEMACHINE => benchmark.fun.stateMachine(p)
            case EFFPI_RUNNER => benchmark.fun.runner(p)
            case AKKA => benchmark.fun.akka(p)
            case unsupported => {
              throw new RuntimeException(s"Unsupported system: ${unsupported}")
            }
          }
          sql"insert into benchmark_duration (`benchmark_id`, `benchmark_type`, `repetition`, `nanoseconds`) values (${benchId}, ${BENCH_SIZE_TIME}, ${r}, ${nanosecs})".update.apply()
        }
        sql"update benchmark set `end` = ${System.currentTimeMillis} where `id` = ${benchId}".update.apply()
      }
    }
  }

  def benchSizeVsMemory(benchGroupId: Long,
                        benchName: String, system: String,
                        db: DB,
                        reduced: Boolean = false) = {
    import javax.management.{Notification, NotificationEmitter, NotificationListener}
    import javax.management.openmbean.CompositeData
    import com.sun.management.{GarbageCollectionNotificationInfo => GCNInfo}
    import java.lang.management.GarbageCollectorMXBean
    import scala.collection.mutable.ListBuffer

    if (!sizeBenchmarks.keySet.contains(benchName)) {
      throw new RuntimeException(s"Unsupported benchmark: ${benchName}")
    }

    println(s"Running benchmark (size vs. memory): ${benchName}; system: ${system}")

    val benchmark = sizeBenchmarks(benchName)
    val params = if (reduced) benchmark.params.take(2) else benchmark.params

    val gcBeans = java.lang.management.ManagementFactory.getGarbageCollectorMXBeans().asScala.toList
    println(s"Garbage collector beans: ${gcBeans.map(_.getName)}")

    // Will contain a map from GCs to memory usage values (in bytes)
    val usedMem = Map((gcBeans.map { b => (b, ListBuffer[Long]()) }):_*)

    val listener = new NotificationListener() {
      override def handleNotification(n: Notification, emitter: Object) = {
        if (n.getType.equals(GCNInfo.GARBAGE_COLLECTION_NOTIFICATION)) {
          val gc = emitter.asInstanceOf[GarbageCollectorMXBean]
          val info = GCNInfo.from(n.getUserData.asInstanceOf[CompositeData])
          val usage = info.getGcInfo.getMemoryUsageBeforeGc
          usedMem(gc) += usage.values.asScala.map(_.getUsed).fold(0L)((x,y) => x+y)
        }
      }
    }

    def attachGCListener() = {
      gcBeans.foreach { g =>
        val emitter = g.asInstanceOf[NotificationEmitter]
        emitter.addNotificationListener(listener, null, emitter)
      }
    }

    def detachGCListener() = {
      gcBeans.foreach { g =>
        val emitter = g.asInstanceOf[NotificationEmitter]
        emitter.removeNotificationListener(listener)
      }
    }

    def resetUsedMem() = {
      usedMem.values.foreach(_.clear())
    }

    implicit val session = db.autoCommitSession()

    params.foreach { p =>
      val benchId = sql"insert into benchmark(`group`, `type`, `name`, `system`, `start`) values (${benchGroupId}, ${BENCH_SIZE_MEMORY}, ${benchName}, ${system}, ${System.currentTimeMillis})".updateAndReturnGeneratedKey.apply()
      benchmark.sqlInsert(benchId, p, session)

      // Memory used before the benchmark
      // val initMem = Runtime.getRuntime.totalMemory-Runtime.getRuntime.freeMemory

      System.gc()
      attachGCListener()
      system match {
        case EFFPI_STATEMACHINE => benchmark.fun.stateMachine(p)
        case EFFPI_RUNNER => benchmark.fun.runner(p)
        case AKKA => benchmark.fun.akka(p)
        case unsupported => {
          throw new RuntimeException(s"Unsupported system: ${unsupported}")
        }
      }

      // Memory used after the benchmark
      // val finalMem = Runtime.getRuntime.totalMemory-Runtime.getRuntime.freeMemory
      // val actualMemUsage = (finalMem - initMem) max 0

      detachGCListener()
      usedMem.foreach { (gc, memUsages) =>
        val gcCalls = memUsages.size
        // If the GC was never invoked, don't save any information
        if (gcCalls != 0) {
          sql"insert into benchmark_memory (`benchmark_id`, `benchmark_type`, `repetition`, `gc`, `calls`, `max_bytes`) values (${benchId}, ${BENCH_SIZE_MEMORY}, 1, ${gc.getName}, ${gcCalls}, ${memUsages.max})".update.apply()
        }
      }
      resetUsedMem()
      sql"update benchmark set `end` = ${System.currentTimeMillis} where `id` = ${benchId}".update.apply()
    }
    session.close()
  }

  // FIXME: the following code needs to be revised & updated
  // def benchTimeVsNumThreads(psIndex: Int, benchmarkIndex: Int, numReps: Int) = {
  //   import effpi.system.ProcessSystemStateMachineMultiStep}
  //   import effpi.benchmarks.{effpi => effpib, akka => akkab}

  //   val pssC = List(
  //     ("runnerimproved", (tpc: Int) => (() => ProcessSystemRunnerImproved(tpc))),
  //     ("statemachinemultistep", (tpc: Int) => (() => ProcessSystemStateMachineMultiStep()))
  //   )

  //   val benchmarks = List(
  //     ("chameneos", effpib.Chameneos.bench((20,40), _: () => ProcessSystemStateMachineMultiStep())),
  //     ("countingactor", effpib.CountingActor.bench(100, _: () => ProcessSystemStateMachineMultiStep())),
  //     ("forkjoincreation", effpib.ForkJoinCreation.bench(50, _: () => ProcessSystemStateMachineMultiStep())),
  //     ("forkjointhroughput", effpib.ForkJoinThroughput.bench((50,50), _: () => ProcessSystemStateMachineMultiStep())),
  //     ("pingpong", PingPong.bench((30,50), _: () => ProcessSystemStateMachineMultiStep())),
  //     ("threadring", Ring.bench((30,60,1), _: () => ProcessSystemStateMachineMultiStep()))
  //   )

  //   val threadPerCoreList = List(1,2,5,10,25,50)

  //   val reps = (1 to numReps).toList

  //   val (psName, ps) = pssC(psIndex)
  //   val (benchname, bench) = benchmarks(benchmarkIndex)
  //   val filePath = s"./benchmarkresults/threads/${benchname}_${psName}.csv"

  //   println(s"$benchname - $psName")

  //   val outputFile = new BufferedWriter(new FileWriter(filePath))
  //   val csvWriter = new CSVWriter(outputFile)

  //   threadPerCoreList.foreach { tpc =>
  //     val line = reps.map { _ =>
  //       bench(ps(tpc)).toString
  //     }.toArray.+:(tpc.toString)
  //     csvWriter.writeNext(line:_*)
  //     csvWriter.flush()
  //   }

  //   outputFile.close()
  // }
}
