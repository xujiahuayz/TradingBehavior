import matplotlib.pyplot as plt
import matplotlib.colors as clrs

import numpy as np

from learning.funcsfinal import fint, funcf, pmf, pmf_first, tmeans


# mpl.use("pgf")

pgf_with_latex = {  # setup matplotlib to use latex for output
    "pgf.texsystem": "pdflatex",  # change this if using xetex or lautex
    "text.usetex": True,  # use LaTeX to write all text
    "axes.labelsize": 20,
    "font.size": 20,
    "legend.fontsize": 20,  # Make the legend/label fonts
    "xtick.labelsize": 20,  # a little smaller
    "ytick.labelsize": 20,
    "pgf.preamble": [
        r"\usepackage{amsmath,amsfonts,amssymb}",
    ],
}

# mpl.rcParams.update(pgf_with_latex)
mar = 0.03

teemax = 60
tees = range(teemax)

## pmf_first
plt.plot(tees, [pmf_first(t) for t in tees], color="black")

## f(t)
plt.plot(tees, [funcf(t) for t in tees], color="darkgrey")
plt.xlabel("Trading age $t$")
plt.ylabel("Intensity of success arrival $f(t)$")
axes = plt.gca()
axes.set_xlim(0, teemax)
axes.set_ylim(0, 2)
plt.show()
plt.tight_layout()


tmean = []
ptmean = []
cnts = [1, 3, 10, 20, 30]

## given a number of success $n$ and a trading age, the probability of achieving that number of success before that trading age,
## i.e. the cumulative probability of "jump"-age for a give $n$ number of jumps
for n in cnts:
    plt.plot(tees, [1 - pmf(t, n) for t in tees], label="$" + str(n) + "$")
    temp = tmeans(n)
    tmean.extend([temp])
    ptmean.extend([1 - pmf(temp, n)])

leg1 = plt.legend(loc=1, title="$n$", handlelength=0.5)
plt.gca().add_artist(leg1)

sct = plt.scatter(
    tmean, ptmean, marker="D", color="black", label="$\\mathbb{E}[t\\,|\\,N_t=n]$"
)
plt.legend([sct], ["$\\mathbb{E}[t\\,|\\,N_t=n]$"], loc=4, handlelength=0)
plt.xlabel("Trading age $t$")
plt.ylabel("$\\mathbb{P}[\\tau \leq t \\,|\\, N_{\\tau} = n]$")

axes = plt.gca()
axes.set_xlim(0, teemax)
axes.set_ylim(0, 1)
plt.tight_layout()
plt.show()

## given a trading age, the cumulative probability of having n number of successes
colors = [
    [0, "white"],
    [0.2, "goldenrod"],
    [0.4, "darkgoldenrod"],
    [0.5, "black"],
    [0.6, "darkgreen"],
    [0.8, "green"],
    [1, "white"],
]
clrmap = clrs.LinearSegmentedColormap.from_list("", colors)

cnts = range(60)

teemax = 45
tees = np.linspace(start=0, stop=teemax, num=99)
data = np.array([[pmf(tee, n) for tee in tees] for n in cnts])

plt.close("all")
plt.pcolormesh(tees, cnts, data, cmap=clrmap)
plt.plot(tees, [fint(t) for t in tees], color="red", label="$\\mathbb{E}[N_t]$")

plt.xlabel("Trading age $t$")
plt.ylabel("Number of successes $n$")

axes = plt.gca()
axes.set_xlim(0, teemax)
axes.set_ylim(0, np.max(cnts))

plt.colorbar(label="$\\mathbb{P}[N_t < n]$", ticks=np.linspace(0, 1, num=7).round(2))
plt.legend(loc=4)
plt.tight_layout()
plt.show()
# plt.savefig("../figure/probn2" + ".pdf")
