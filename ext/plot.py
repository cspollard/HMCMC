import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
import numpy as np
from sys import stdin, stdout


names = map(str.strip, stdin.readline().split(","))
xs = np.loadtxt(stdin, delimiter=',').transpose()

for n in range(len(xs)):
    param = xs[n]
    name = names[n]
    hist, bins = np.histogram(param, bins=50)
    width = 0.7 * (bins[1] - bins[0])
    center = (bins[:-1] + bins[1:]) / 2

    fig = plt.figure()
    fig.suptitle(name)

    plt.bar(center, hist, align='center', width=width)
    plt.savefig("%s.png" % name)
    plt.clf()

    print name, "median, stddev: %.2f, %.2f" \
            % (np.median(param), np.std(param))
    stdout.flush()

    plt.close()
