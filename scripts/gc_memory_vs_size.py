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
GENERAL_PLOTS_PATH = './graphs/memory/general/'
BAR_PLOTS_PATH = './graphs/memory/'
PS_PLOTS_PATH = './graphs/memory/processsystem/'

Y_AXIS_LABEL = "Max GC memory (MB)"
BENCHNAMES = [
    ("chameneos", "Chameneos: number of chameneos", Y_AXIS_LABEL),
    ("counting", "Counting actors: numbers to add", Y_AXIS_LABEL),
    ("forkjoin_creation", "FJC: number of processes", Y_AXIS_LABEL),
    ("forkjoin_throughput", "FJT: number of processes", Y_AXIS_LABEL),
    ("pingpong", "Ping-pong: number of pairs", Y_AXIS_LABEL),
    ("ring", "Thread ring: number of ring members", Y_AXIS_LABEL),
    ("ringstream", "Thread ring stream: number of ring members", Y_AXIS_LABEL)
]

PSNAMES = [
    ('akka', '-.'),
    ('statemachinemultistep', '--'),
    ('runnerimproved', '-')
]


def gc_usage_vs_size():
    for bn, xl, yl in BENCHNAMES:
        print('Generating size vs. GC usage for benchmark: ' + bn)
        gc_usage_vs_size_per_benchmark(bn, xl, yl)


def gc_usage_vs_size_per_benchmark(benchname, xl, yl):
    (fig_w, fig_h) = (4, 4)
    f = plt.figure(figsize=(fig_w, fig_h))
    gs = plt.GridSpec(2, 1)
    ax1 = plt.subplot(gs[0, :])
    ax2 = plt.subplot(gs[1, :], sharex=ax1)

    points = assemble_data(benchname, PSNAMES)

    for psName, sizes, records, _e, avg_calls, sty in points:
        records = [r / 1000000 for r in records]
        ax1.loglog(sizes, records, marker='o', markersize=6, linestyle=sty)

    for psName, sizes, records, _e, avg_calls, sty in points:
        ax2.loglog(sizes, avg_calls, marker='o', markersize=6, linestyle=sty)

    ax2.set_xscale("log")

    ax2.set_xlabel(xl)
    ax1.get_xaxis().set_visible(False)
    # ax1.set_ylabel(yl, rotation=0)
    # ax2.set_ylabel('Number of GC calls', rotation=0)
    ax1.text(-0.1, 1.15, yl, fontsize=12, transform=ax1.transAxes,
        verticalalignment='top')
    ax2.text(
        -0.1, 1.15, 'Number of GC calls',
        fontsize=12, transform=ax2.transAxes,
        verticalalignment='top'
    )

    f.savefig('{}{}.pdf'.format(BAR_PLOTS_PATH, benchname), bbox_inches='tight')
    plt.close(f)


def gc_calls_vs_size_barchart():
    for bn, xl, yl in BENCHNAMES:
        print('Generating size vs. GC calls bar chart for benchmark: ' + bn)
        gc_calls_vs_size_barchart_per_benchmark(bn, xl, yl)


def gc_calls_vs_size_barchart_per_benchmark(benchname, xl, yl):
    f, ax = plt.subplots()

    points = assemble_data(benchname, PSNAMES)

    # for psName, sizes, records, _e, avg_calls, sty in points:
    #     print(avg_calls)
    points = assemble_data(benchname, PSNAMES)

    for psName, sizes, records, _e, avg_calls, sty in points:
        ax.loglog(sizes, records, marker='o', markersize=3, linestyle=sty)

    psName, sizes, records, _e, avg_calls, sty = points[0]
    w = np.array(sizes) / 4
    ax.bar(sizes - w, avg_calls, width=w, align="edge", color='orange', label=psName)

    psName, sizes, records, _e, avg_calls, sty = points[1]
    w = np.array(sizes) / 4
    ax.bar(sizes, avg_calls, width=w, align="edge", color='green', label=psName)

    psName, sizes, records, _e, avg_calls, sty = points[2]
    w = np.array(sizes) / 4
    ax.bar(sizes + w, avg_calls, width=w, align="edge", color='blue', label=psName)


    plt.xlabel(xl)
    plt.ylabel(yl)

    ax.set_xscale("log")
    # plt.xlabel(xl)
    # plt.ylabel(yl)
    # plt.legend(loc=" left")

    f.savefig('{}{}.pdf'.format(BAR_PLOTS_PATH, benchname), bbox_inches='tight')
    plt.close(f)


def plot_memory_vs_size_general():
    for bn, xl, yl in BENCHNAMES:
        print('Generating size vs. GC memory plot for benchmark: ' + bn)
        plot_memory_vs_size_general_per_benchmark(bn, xl, yl)


def plot_memory_vs_size_general_per_benchmark(benchname, xl, yl):
    f, ax = plt.subplots()

    points = assemble_data(benchname, PSNAMES)

    for psName, sizes, records, _e, avg_calls, sty in points:
        ax.loglog(sizes, records, marker='o', markersize=3, linestyle=sty)


    plt.xlabel(xl)
    plt.ylabel(yl)
    # plt.legend(loc="upper left")

    f.savefig('{}{}.pdf'.format(GENERAL_PLOTS_PATH, benchname), bbox_inches='tight')
    plt.close(f)


def assemble_data(benchname, PSNAMES, gid = None):
    points = []

    for psName, sty in PSNAMES:
        sizes, avg_records, e, _raw_records, avg_calls = fetch_data(benchname, psName)
        points.append((psName, sizes, avg_records, e, avg_calls, sty))

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
                  "AND benchmark.`type` = 'size_vs_memory'" % {
                      'b' : 'benchmark_' + bench_name,
                      'f' : size_field
                  },
                  (gid, bench_name, system))
        id_sizes = c.fetchall()
        bench_ids = [r['id'] for r in id_sizes]
        sizes = [r[size_field] for r in id_sizes]

        records = [c.execute("SELECT `max_bytes` "
                                      "FROM benchmark_memory "
                                      "WHERE `benchmark_id` = ?",
                                      (bid,)).fetchall()
                   for bid in bench_ids]

        calls = [c.execute("SELECT `calls` "
                                      "FROM benchmark_memory "
                                      "WHERE `benchmark_id` = ?",
                                      (bid,)).fetchall()
                   for bid in bench_ids]

        # avg_records = [np.average(r) for r in records]
        # errors = [np.std(r) for r in records]
        # avg_calls = [int(np.average(c)) for c in calls]
        avg_records = [empty_safe_avg(r) for r in records]
        avg_calls = [empty_safe_avg(c) for c in calls]
        # errors = empty_safe_std(records)
        # avg_calls = empty_safe_avg(calls)
        # avg_records = [1 for r in records]
        errors =[empty_safe_std(r) for r in records]
        # avg_calls =[1 for r in records]

        return filter_out_empty_records(
            sizes, avg_records, errors, records, avg_calls)


def empty_safe_avg(ls):
    return np.average(ls) if ls else -1


def empty_safe_std(ls):
    return np.std(ls) if ls else -1


def filter_out_empty_records(sizes, avg_rs, errs, rs, avg_calls):
    all_info = list(zip(sizes, avg_rs, errs, rs, avg_calls))
    filtered_info = [info for info in all_info if info[1] != -1]
    unziped_info = list(zip(*filtered_info))
    return tuple([list(ui) for ui in unziped_info])


def main():
    # plot_memory_vs_size_general()
    # gc_calls_vs_size_barchart()
    gc_usage_vs_size()

    # plot_memory_vs_size_error_bar()

if __name__ == "__main__":
    main()
