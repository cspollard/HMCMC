import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
import numpy as np
from sys import stdin


xs = np.fromfile(stdin, sep='\n')

hist, bins = np.histogram(xs, bins=50)
width = 0.7 * (bins[1] - bins[0])
center = (bins[:-1] + bins[1:]) / 2

plt.bar(center, hist, align='center', width=width)
plt.show()
