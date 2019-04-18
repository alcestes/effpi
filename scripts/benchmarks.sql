-- Effpi - verified message-passing programs in Dotty
-- Copyright 2019 Alceste Scalas and Elias Benussi
-- Released under the MIT License: https://opensource.org/licenses/MIT

-- SQLite DB schema for benchmarking scripts
-- Alceste Scalas <alceste.scalas@imperial.ac.uk>

PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS benchmark_group (
  `id` INTEGER PRIMARY KEY,
  `description` TEXT,
  `start` INTEGER NOT NULL, -- Unix timestamp: millisecs since epoch
  `end` INTEGER             -- NULL if benchmark is not completed
);

CREATE TABLE IF NOT EXISTS benchmark (
  `id` INTEGER NOT NULL PRIMARY KEY,
  `group` INTEGER NOT NULL REFERENCES benchmark_group(`id`)
                           ON UPDATE CASCADE ON DELETE RESTRICT,
  `type` VARCHAR(50) NOT NULL,
  `name` VARCHAR(50) NOT NULL,
  `system` VARCHAR(50) NOT NULL, -- System under benchmark (e.g., Effpi, Akka)
  `start` INTEGER NOT NULL, -- Must be a Unix timestamp, since epoch
  `end` INTEGER,            -- NULL if benchmark is not completed

  UNIQUE (`id`, `type`, `name`)
  UNIQUE (`id`, `name`)
  UNIQUE (`id`, `type`)
  CHECK(`type` == 'size_vs_time' OR `type` == 'size_vs_memory')
  CHECK(`name` == 'chameneos' OR
        `name` == 'counting' OR
        `name` == 'pingpong' OR
        `name` == 'forkjoin_creation' OR
        `name` == 'forkjoin_throughput' OR
        `name` == 'ring' OR
        `name` == 'ringstream')
);

--- Chameneos (size vs. time)
CREATE TABLE IF NOT EXISTS benchmark_chameneos (
  `id` INTEGER PRIMARY KEY,
  `name` VARCHAR(50) NOT NULL,

  `size` INTEGER NOT NULL,
  `meetings` INTEGER NOT NULL,

  FOREIGN KEY (`id`, `name`)
      REFERENCES benchmark(`id`, `name`)
      ON UPDATE CASCADE ON DELETE RESTRICT,
  CHECK (`name` == 'chameneos')
);

--- Chameneos (size vs. time)
CREATE TABLE IF NOT EXISTS benchmark_counting (
  `id` INTEGER PRIMARY KEY,
  `name` VARCHAR(50) NOT NULL,

  `count` INTEGER NOT NULL,
  
  FOREIGN KEY (`id`, `name`)
      REFERENCES benchmark(`id`, `name`)
      ON UPDATE CASCADE ON DELETE RESTRICT,
  CHECK (`name` == 'counting')
);
--- Ping-pong (size vs. time)
CREATE TABLE IF NOT EXISTS benchmark_pingpong (
  `id` INTEGER PRIMARY KEY,
  `name` VARCHAR(50) NOT NULL,

  `pairs` INTEGER NOT NULL,
  `exchanges` INTEGER NOT NULL,

  FOREIGN KEY (`id`, `name`)
      REFERENCES benchmark(`id`, `name`)
      ON UPDATE CASCADE ON DELETE RESTRICT,
  CHECK (`name` == 'pingpong')
);

--- Fork/join creation (size vs. time)
CREATE TABLE IF NOT EXISTS benchmark_forkjoin_creation (
  `id` INTEGER PRIMARY KEY,
  `name` VARCHAR(50) NOT NULL,

  `size` INTEGER NOT NULL,

  FOREIGN KEY (`id`, `name`)
      REFERENCES benchmark(`id`, `name`)
      ON UPDATE CASCADE ON DELETE RESTRICT,
  CHECK (`name` == 'forkjoin_creation')
);

--- Fork/join throughput (size vs. time)
CREATE TABLE IF NOT EXISTS benchmark_forkjoin_throughput (
  `id` INTEGER PRIMARY KEY,
  `name` VARCHAR(50) NOT NULL,

  `size` INTEGER NOT NULL,
  `messages` INTEGER NOT NULL,

  FOREIGN KEY (`id`, `name`)
      REFERENCES benchmark(`id`, `name`)
      ON UPDATE CASCADE ON DELETE RESTRICT,
  CHECK (`name` == 'forkjoin_throughput')
);

--- Ring (size vs. time)
CREATE TABLE IF NOT EXISTS benchmark_ring (
  `id` INTEGER PRIMARY KEY,
  `name` VARCHAR(50) NOT NULL,

  `size` INTEGER NOT NULL,
  `hops` INTEGER NOT NULL,
  
  FOREIGN KEY (`id`, `name`)
      REFERENCES benchmark(`id`, `name`)
      ON UPDATE CASCADE ON DELETE RESTRICT,
  CHECK (`name` == 'ring')
);

--- Streaming ring (size vs. time)
CREATE TABLE IF NOT EXISTS benchmark_ringstream (
  `id` INTEGER PRIMARY KEY,
  `name` VARCHAR(50) NOT NULL,

  `size` INTEGER NOT NULL,
  `hops` INTEGER NOT NULL,
  `messages` INTEGER NOT NULL,

  FOREIGN KEY (`id`, `name`)
      REFERENCES benchmark(`id`, `name`)
      ON UPDATE CASCADE ON DELETE RESTRICT,
  CHECK (`name` == 'ringstream')
);

-- Benchmark result, for any benchmark measuring a duration
CREATE TABLE IF NOT EXISTS benchmark_duration (
  `benchmark_id` INTEGER NOT NULL REFERENCES benchmark('id')
                                  ON UPDATE CASCADE ON DELETE RESTRICT,
  `benchmark_type` VARCHAR(50) NOT NULL,

  `repetition` INTEGER NOT NULL,
  `nanoseconds` UNSIGNED BIG INT,

  PRIMARY KEY (`benchmark_id`, `repetition`),
  FOREIGN KEY (`benchmark_id`, `benchmark_type`)
      REFERENCES benchmark(`id`, `type`)
      ON UPDATE CASCADE ON DELETE RESTRICT,
  CHECK(`benchmark_type` == 'size_vs_time')
);

-- Benchmark result, for any benchmark measuring memory usage
CREATE TABLE IF NOT EXISTS benchmark_memory (
  `benchmark_id` INTEGER NOT NULL REFERENCES benchmark('id')
                                  ON UPDATE CASCADE ON DELETE RESTRICT,
  `benchmark_type` VARCHAR(50) NOT NULL,

  `repetition` INTEGER NOT NULL,
  `gc` VARCHAR(255) NOT NULL,    -- Name of the GC bean emitting notifications
  `calls` UNSIGNED BIG NOT NULL, -- Number of GC notifications emitted
  `max_bytes` UNSIGNED BIG INT NOT NULL, -- Maximum used memory

  PRIMARY KEY (`benchmark_id`, `repetition`, `gc`),
  FOREIGN KEY (`benchmark_id`, `benchmark_type`)
      REFERENCES benchmark(`id`, `type`)
      ON UPDATE CASCADE ON DELETE RESTRICT,
  CHECK(`benchmark_type` == 'size_vs_memory')
);
