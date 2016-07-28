import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
import numpy as np
from sys import stdin, stdout


names = map(str.strip, stdin.readline().split(","))
xs = np.loadtxt(stdin, delimiter=',').transpose()

n = 0
for param in xs:
    hist, bins = np.histogram(param, bins=50)
    width = 0.7 * (bins[1] - bins[0])
    center = (bins[:-1] + bins[1:]) / 2

    fig = plt.figure()
    fig.suptitle(names[n])

    plt.bar(center, hist, align='center', width=width)
    plt.savefig("param%03i.png" % n)
    plt.clf()

    print names[n]
    print "5%%, 50%%, 95%%: %.2f, %.2f, %.2f" \
            % (np.percentile(param, 5), np.median(param),
                    np.percentile(param, 95))
    stdout.flush()
    n += 1
