#!/usr/bin/env python3
# Effpi - verified message-passing programs in Dotty
# Copyright 2019 Alceste Scalas and Elias Benussi
# Released under the MIT License: https://opensource.org/licenses/MIT
import itertools
import operator
from ast import literal_eval
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter

SQLITE_FILE = '../benchmarks.db'
GENERAL_PLOTS_PATH = './graphs/time/'
THREAD_PLOTS_PATH = './graphs/threadpercore/'
PS_PLOTS_PATH = './graphs/processsystem/'

DATA_SIZE_PATH = '../benchmarkresults/size/'
DATA_TPC_PATH = '../benchmarkresults/threads/'

BENCHNAMES = [
    ("chameneos", "Number of chameneos", "Time (ms)"),
    ("counting", "Numbers to add", "Time (ms)"),
    ("forkjoin_creation", "Number of processes", "Time (ms)"),
    ("forkjoin_throughput", "Number of processes", "Time (ms)"),
    ("pingpong", "Number of pairs", "Time (ms)"),
    ("ring", "Number of ring members", "Time (ms)"),
    ("ringstream", "Number of ring members", "Time (ms)")
]

PSNAMES = [
    ('akka', 'Akka Typed', '-.'),
    ('statemachinemultistep', 'Effpi with channel FSM', '--'),
    ('runnerimproved', 'Effpi default', '-')
]


def plot_time_vs_size_general():
    for bn, xl, yl in BENCHNAMES:
        print('Generating size vs. time plot for benchmark: ' + bn)
        plot_time_vs_size_general_per_benchmark(bn, xl, yl)


def plot_time_vs_size_general_per_benchmark(benchname, xl, yl):
    f, ax = plt.subplots(figsize=(3.5, 3.5))
    # ax.axis([1, 100000, 1, 1000000000000])

    # NOTE: using specific GID
    points = assemble_data(benchname, PSNAMES)

    for psName, sizes, records, _e, psLabel, sty in points:
        ax.loglog(sizes, records, marker='o', markersize=6, label=psLabel, linestyle=sty)

    # This allows to have log scale with normal values
    # ax.xaxis.set_major_formatter(ScalarFormatter())
    # ax.yaxis.set_major_formatter(ScalarFormatter())

    plt.xlabel(xl, fontsize=12)
    plt.ylabel(yl, fontsize=12)
    plt.legend(loc="upper left")

    f.savefig('{}{}.pdf'.format(GENERAL_PLOTS_PATH, benchname), bbox_inches='tight')
    plt.close(f)


def assemble_data(benchname, PSNAMES, gid = None):
    points = []

    for psName, psLabel, sty in PSNAMES:
        sizes, avg_records, e, _raw_records = fetch_data(benchname, psName, gid)
        points.append((psName, sizes, avg_records, e, psLabel, sty))

    return points


def fetch_data(bench_name, system, gid = None):
    import sqlite3
    with sqlite3.connect('file:{}?mode=ro'.format(SQLITE_FILE), uri=True) as conn:
        conn.row_factory = sqlite3.Row
        c = conn.cursor()

        if gid is None:
            # Select the latest benchmark group id
            c.execute("SELECT `id` FROM benchmark_group "
                      "WHERE `end` IS NOT NULL "
                      "ORDER BY `end` DESC LIMIT 1")
            gid = c.fetchone()['id']

        # Which DB field is the "size", for the x-axis of the plot?
        size_fields = {
            'chameneos' : 'size',
            'counting' : 'count',
            'pingpong' : 'pairs',
            'forkjoin_creation' : 'size',
            'forkjoin_throughput' : 'size',
            'ring' : 'size',
            'ringstream' : 'size'
        }
        size_field = size_fields[bench_name]

        # Select id,size pairs for all benchmkars with given name and group
        c.execute("SELECT benchmark.`id`, %(b)s.`%(f)s` "
                  "FROM benchmark "
                  "INNER JOIN %(b)s "
                  "ON (benchmark.`id` = %(b)s.`id`) "
                  "WHERE benchmark.`group` = ? AND benchmark.`name` = ? "
                  "AND benchmark.`system` = ? "
                  "AND benchmark.`type` = 'size_vs_time'" % {
                      'b' : 'benchmark_' + bench_name,
                      'f' : size_field
                  },
                  (gid, bench_name, system))
        id_sizes = c.fetchall()
        bench_ids = [r['id'] for r in id_sizes]
        sizes = [r[size_field] for r in id_sizes]

        tomillisecs = lambda r: r['nanoseconds'] / 1000000
        records = [list(map(tomillisecs,
                            c.execute("SELECT `nanoseconds` "
                                      "FROM benchmark_duration "
                                      "WHERE `benchmark_id` = ?",
                                      (bid,)).fetchall()))
                   for bid in bench_ids]

        avg_records = [empty_safe_avg(r) for r in records]
        errors = [empty_safe_std(r) for r in records]

        # return sizes, avg_records, errors, records
        return filter_out_empty_records(sizes, avg_records, errors, records)


def empty_safe_avg(ls):
    return np.average(ls) if ls else -1


def empty_safe_std(ls):
    return np.std(ls) if ls else -1


def filter_out_empty_records(sizes, avg_rs, errs, rs):
    all_info = list(zip(sizes, avg_rs, errs, rs))
    filtered_info = [info for info in all_info if info[1] != -1]
    unziped_info = list(zip(*filtered_info))
    return tuple([list(ui) for ui in unziped_info])


def main():
    plot_time_vs_size_general()


if __name__ == "__main__":
    main()
