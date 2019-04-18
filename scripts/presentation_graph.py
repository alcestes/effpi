# Effpi - verified message-passing programs in Dotty
# Copyright 2019 Alceste Scalas and Elias Benussi
# Released under the MIT License: https://opensource.org/licenses/MIT
import csv
import itertools
import operator
from ast import literal_eval
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter

GENERAL_PLOTS_PATH = './graphs/presentation/'

DATA_SIZE_PATH = '../benchmarkresults/size/'

BENCHNAMES = [
    ("chameneos", "Number of chameneos", "Time (milliseconds)"),
    ("countingactor", "Numbers to add", "Time (milliseconds)"),
    ("forkjoincreation", "Number of processes", "Time (milliseconds)"),
    ("forkjointhroughput", "Number of processes", "Time (milliseconds)"),
    ("pingpong", "Number of pairs", "Time (milliseconds)"),
    ("threadring", "Number of ring members", "Time (milliseconds)")]

OPTIMISATIONS = [
    ('runningqueue',':'),
    ('waitqueueimproved', '-')
]

PSNAMES = [('original', '--')] + OPTIMISATIONS

def plot_time_vs_size_general():
    for bn, xl, yl in BENCHNAMES:
        plot_time_vs_size_general_per_benchmark(bn, xl, yl)


def plot_time_vs_size_general_per_benchmark(benchname, xl, yl):
    f, ax = plt.subplots()
    # ax.axis([1, 100000, 1, 1000000000000])

    points = assemble_data(benchname, PSNAMES)

    for psName, sizes, records, e, sty in points:
        ax.loglog(sizes, records, marker='o', markersize=3, label=psName, linestyle=sty)

    # This allows to have log scale with normal values
    # ax.xaxis.set_major_formatter(ScalarFormatter())
    # ax.yaxis.set_major_formatter(ScalarFormatter())

    plt.xlabel(xl)
    plt.ylabel(yl)
    plt.legend()

    f.savefig('{}{}.pdf'.format(GENERAL_PLOTS_PATH, benchname), bbox_inches='tight')
    plt.close(f)


def assemble_data(benchname, PSNAMES):
    points = []

    for psName, sty in PSNAMES:
        sizes, avg_records, e, raw_records = fetch_data(
            '{}{}_{}.csv'.format(DATA_SIZE_PATH, benchname, psName))
        points.append((psName, sizes, avg_records, e, sty))

    return points


def fetch_data(filename):
    with open(filename, newline='') as f:
        csv_reader = csv.reader(f, delimiter=',', quotechar='"')

        records = [row for row in csv_reader]

        # Convert sizes to numerical or tuple
        sizes = [literal_eval(r[0]) for r in records]
        if (type(sizes[0]) != int):
            sizes = [h[0] for h in sizes]

        # Convert records to numerical, and from nsecs to millisecs
        records = [r[1:] for r in records]
        toint = lambda xs: [int(x) / 1000000 for x in xs]
        records = [toint(record) for record in records]

        avg_records = [np.average(r) for r in records]
        errors = [np.std(r) for r in records]

        return sizes, avg_records, errors, records


def main():
    plot_time_vs_size_general()

if __name__ == "__main__":
    main()
