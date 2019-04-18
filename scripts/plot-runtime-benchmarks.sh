#!/bin/bash

cd scripts
mkdir -p graphs/time
mkdir -p graphs/memory
python3 state_machine_graph.py
python3 gc_memory_vs_size.py
