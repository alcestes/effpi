#!/bin/bash

REPETITIONS=${1:-1}

sbt ";clean;benchmarks/clean"
sbt benchmarks/assembly

./scripts/runBenchmarks $REPETITIONS
rm -f hs_err*.log
