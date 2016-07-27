import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
import numpy as np
from sys import stdin


xs = np.loadtxt(stdin, delimiter=',').transpose()
print xs.shape

n = 1
for param in xs:
    hist, bins = np.histogram(param, bins=50)
    width = 0.7 * (bins[1] - bins[0])
    center = (bins[:-1] + bins[1:]) / 2

    plt.bar(center, hist, align='center', width=width)
    plt.savefig("param%03i.png" % n)
    plt.clf()
    n += 1
