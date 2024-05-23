import importlib
from learning.constants import DATA_DIR, FIG_DIR
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.colors as clrs


import pickle
import multiprocessing

import numpy as np

from learning.funcsfinal import fint, funcf, pi_ell, pmf, tmeans


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

plt.close("all")
plt.plot(tees, [funcf(t) for t in tees], color="darkgrey")
plt.xlabel("Trading age $t$")
plt.ylabel("Intensity of success arrival $f(t)$")
axes = plt.gca()
axes.set_xlim(0, teemax)
axes.set_ylim(0, 2)
plt.show()
plt.tight_layout()

plt.savefig(DATA_DIR / "funcf2.pdf")


tmean = []
ptmean = []
cnts = [1, 3, 10, 20, 30]

plt.close("all")
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
# plt.savefig("../figure/cdft2" + ".pdf")


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


colors = [[0, "lightgrey"], [1, "black"]]
clrmap = clrs.LinearSegmentedColormap.from_list("", colors)
pimax = 0.98
pi0value = np.linspace(start=pimax, stop=1 - pimax, num=7)
norm = clrs.Normalize(1 - pimax, pimax)
plt.close("all")
for pi0 in pi0value:
    col = clrmap(norm(pi0))
    plt.plot(tees, [pi_ell(tee, pi0) for tee in tees], color=col)
plt.xlabel("Trading age $t$")
plt.ylabel("Belief $\\pi(t)$")
axes = plt.gca()
axes.set_xlim(0, teemax)
axes.set_ylim(0, 1)
sm = plt.cm.ScalarMappable(cmap=clrmap, norm=plt.Normalize(vmin=1 - pimax, vmax=pimax))
# plt.colorbar(sm, ticks=pi0value, label="Prior belief $\\pi_0$")
plt.tight_layout()
plt.show()
# plt.savefig("../figure/pis2" + ".pdf")


pimax = 0.8
pi0value = np.linspace(start=pimax, stop=1 - pimax, num=3)
phimax = 1.21
phivalue = np.linspace(start=0.01, stop=phimax, num=3)
teemax = 3
teevalue = np.linspace(start=teemax, stop=0, num=5)

taumax = 5
tauns = np.linspace(start=0, stop=taumax, num=70)
norm = clrs.Normalize(0, teemax)
# exit -> value through time
for pi0 in pi0value:
    plt.close("all")
    for tee in teevalue:
        col = clrmap(norm(tee))
        taus = tauns[tauns > tee]
        plt.plot(taus, [pdetaun(tau, pi0, tee) for tau in taus], color=col, marker=".")
    plt.xlabel("$\\mathcal{P}(\\tau \\,|\\, t)$")
    plt.ylabel("$\\tau$")
    axes = plt.gca()
    axes.set_xlim([0, taumax])
    axes.set_ylim([0, 1.3])
    sm = plt.cm.ScalarMappable(cmap=clrmap, norm=plt.Normalize(vmin=0, vmax=teemax))
    plt.colorbar(sm, ticks=teevalue, label="Trading age $t$")
    plt.text(
        mar,
        1 - mar,
        str("$\\pi_0 = " + str(round(pi0, 2)) + "$"),
        horizontalalignment="left",
        verticalalignment="top",
        transform=axes.transAxes,
    )
    plt.tight_layout()
    plt.show()
    # plt.savefig('../figure/pdf' +
    #             'pi' + list(pi0value).index(pi0).__str__() + '.pdf')


pimax = 0.8
pi0value = np.linspace(start=pimax, stop=1 - pimax, num=3)
phimax = 1.21
phivalue = np.linspace(start=0.01, stop=phimax, num=3)
teemax = 3
teevalue = np.linspace(start=teemax, stop=0, num=5)

taumax = 5
tauetry = np.linspace(start=0, stop=taumax, num=70)
norm = clrs.Normalize(0, teemax)
# exit -> value through time
for phi in phivalue:
    for pi0 in pi0value:
        plt.close("all")
        tauu = tauuroot(pi0, phi)
        vopt = [0]
        eopt = [tauu]
        for tee in teevalue[teevalue < tauu]:
            col = clrmap(norm(tee))
            taus = tauetry[tauetry > tee]
            vestemp = [ve(pi0, tee, phi, taue) for taue in taus]

            tauestar = taueroot(tee, pi0, phi)
            eopt.extend([tauestar])
            vopt.extend([ve(pi0, tee, phi, tauestar)])

            plt.plot(taus, vestemp, color=col, marker=".")
            plt.xlabel("Exit time $\\tau_E$")
            plt.ylabel("Expected value $\\mathbb{E}[V]$")

        plt.plot(eopt, vopt, color="red", marker=".", label="$\\tau_E^*$")
        axes = plt.gca()
        axes.set_xlim([0, taumax])
        axes.set_ylim([0, 12])
        sm = plt.cm.ScalarMappable(cmap=clrmap, norm=plt.Normalize(vmin=0, vmax=teemax))
        plt.colorbar(sm, ticks=teevalue, label="Trading age $t$")
        plt.text(
            mar,
            1 - mar,
            str(
                "$\\pi_0 = "
                + str(round(pi0, 2))
                + "$\n$\\varphi = "
                + str(round(phi, 2))
                + "$"
            ),
            horizontalalignment="left",
            verticalalignment="top",
            transform=axes.transAxes,
        )
        plt.legend(bbox_to_anchor=(1, 0), loc=2, borderaxespad=0.7, frameon=False)
        plt.tight_layout()
        plt.show()
        # plt.savefig('../figure/exi' + 'phi' + list(phivalue).index(phi).__str__() +
        #             'pi' + list(pi0value).index(pi0).__str__() + '.pdf')


for c in cvalue:
    phimax = 1.21
    colors = [[0, "lightgrey"], [1, "black"]]
    clrmap = clrs.LinearSegmentedColormap.from_list("", colors)
    if c > 0:
        pimax = 0.8
        norm = clrs.Normalize(1 - pimax, pimax)
        ## eta
        teemax = 3
        teevalue = np.linspace(start=0, stop=teemax, num=7)
        phivalue = np.linspace(start=0.01, stop=phimax, num=3)
        pi0value = np.linspace(start=pimax, stop=1 - pimax, num=3)
        taumax = 5

        norm = clrs.Normalize(0, teemax)
        for phi in phivalue:
            for pi0 in pi0value:
                if pi0 <= pi0min(phi, c):
                    break
                else:
                    plt.close("all")
                    etaes = []
                    taues = []
                    tauu = tauuroot(pi0, phi, c)
                    tees = teevalue[teevalue < tauu]
                    for tee in tees:
                        col = clrmap(norm(tee))
                        taue = taueroot(tee, pi0, phi, c)
                        taues.extend([taue])
                        etaes.extend([etae(taue, tee, pi0, c)])
                        taus = np.arange(start=tee, stop=taue, step=0.05)
                        etatemp = [eta(tau, tee, phi, pi0, c) for tau in taus]

                        plt.plot(taus, etatemp, color=col, marker=".", zorder=1)
                        plt.xlabel(
                            "Future trading age $\\tau, t \\leq \\tau \\leq \\tau_E^*(t)$"
                        )
                        plt.ylabel("Effort $\\eta(\\tau \\,| \\, t)$")

                    plt.scatter(
                        tees,
                        [eta(tee, tee, phi, pi0, c) for tee in tees],
                        color="red",
                        marker=".",
                        label="$\\eta(t\\,|\\,t)$",
                        zorder=2,
                    )
                    plt.scatter(
                        taues,
                        etaes,
                        color="blue",
                        marker=".",
                        label="$\\eta(\\tau_E^*(t) \\,|\\,t)$",
                        zorder=2,
                    )
                    axes = plt.gca()
                    axes.set_xlim([0, taumax])
                    axes.set_ylim([0, 1])
                    sm = plt.cm.ScalarMappable(
                        cmap=clrmap, norm=plt.Normalize(vmin=0, vmax=teemax)
                    )
                    plt.colorbar(sm, ticks=teevalue, label="Trading age $t$")
                    plt.text(
                        mar,
                        1 - mar,
                        str(
                            "$\\pi_0 = "
                            + str(round(pi0, 2))
                            + "$\n$\\varphi = "
                            + str(round(phi, 2))
                            + "$"
                        ),
                        horizontalalignment="left",
                        verticalalignment="top",
                        transform=axes.transAxes,
                    )
                    plt.legend(loc=1)
                    plt.tight_layout()
                    # plt.savefig('../figure/etac' + list(cvalue).index(c).__str__() +
                    #             'phi' + list(phivalue).index(phi).__str__() +
                    #             'pi' + list(pi0value).index(pi0).__str__() + '.pdf')
                    plt.show()

        pimax = 0.98
        pi0value = np.linspace(start=pimax, stop=1 - pimax, num=7)
        taumax = 5

        norm = clrs.Normalize(1 - pimax, pimax)
        for phi in phivalue:
            plt.close("all")
            for pi0 in pi0value:
                if pi0 <= pi0min(phi, c):
                    break
                else:
                    col = clrmap(norm(pi0))
                    tauu = tauuroot(pi0, phi, c)
                    tees = np.arange(start=0, stop=tauu, step=0.1)
                    plt.plot(
                        tees,
                        [eta(tee, tee, phi, pi0, c) for tee in tees],
                        color=col,
                        marker=".",
                    )
            plt.xlabel("Trading age $t$")
            plt.ylabel("Actual effort $\\eta(t \\,| \\, t)$")
            axes = plt.gca()
            axes.set_xlim([0, taumax])
            axes.set_ylim([0, 1.2])
            sm = plt.cm.ScalarMappable(
                cmap=clrmap, norm=plt.Normalize(vmin=1 - pimax, vmax=pimax)
            )
            plt.colorbar(sm, ticks=pi0value, label="Prior belief $\\pi_0$")
            plt.text(
                mar,
                1 - mar,
                str("$\\varphi = " + str(round(phi, 2)) + "$"),
                horizontalalignment="left",
                verticalalignment="top",
                transform=axes.transAxes,
            )
            plt.tight_layout()
            plt.show()
            # plt.savefig('../figure/actetac' + list(cvalue).index(c).__str__() +
            #                 'phi' + list(phivalue).index(phi).__str__() + '.pdf')

        pimax = 0.98
        pi0value = np.linspace(start=pimax, stop=1 - pimax, num=3)
        phivalue = np.linspace(start=0.01, stop=phimax, num=7)
        taumax = 5

        norm = clrs.Normalize(1 - phimax, phimax)
        for pi0 in pi0value:
            plt.close("all")
            for phi in phivalue:
                if pi0 <= pi0min(phi, c):
                    break
                else:
                    col = clrmap(norm(phi))
                    tauu = tauuroot(pi0, phi, c)
                    tees = np.arange(start=0, stop=tauu, step=0.1)
                    plt.plot(
                        tees,
                        [eta(tee, tee, phi, pi0, c) for tee in tees],
                        color=col,
                        marker=".",
                    )
            plt.xlabel("Trading age $t$")
            plt.ylabel("Actual effort $\\eta(t \\,| \\, t)$")
            axes = plt.gca()
            axes.set_xlim([0, taumax])
            axes.set_ylim([0, 1.2])
            sm = plt.cm.ScalarMappable(
                cmap=clrmap, norm=plt.Normalize(vmin=1 - phimax, vmax=phimax)
            )
            plt.colorbar(sm, ticks=phivalue, label="Running cost $\\varphi$")
            plt.text(
                mar,
                1 - mar,
                str("$\\pi_0 = " + str(round(pi0, 2)) + "$"),
                horizontalalignment="left",
                verticalalignment="top",
                transform=axes.transAxes,
            )
            plt.tight_layout()
            plt.show()

    pimax = 0.98
    phivalue = np.linspace(start=0.01, stop=phimax, num=3)
    norm = clrs.Normalize(1 - pimax, pimax)
    # time -> optimal exit time
    teemax = 8
    teevalue = np.linspace(start=0, stop=teemax, num=40)
    pi0value = np.linspace(start=pimax, stop=1 - pimax, num=7)
    for phi in phivalue:
        plt.close("all")
        taueult = []
        for pi0 in pi0value:
            col = clrmap(norm(pi0))
            try:
                tauu = tauuroot(pi0, phi, c=c)
                tees = teevalue[teevalue < tauu]
                taueult.extend([tauu])
                plt.plot(
                    tees,
                    [taueroot(tee, pi0, phi, c=c) for tee in tees],
                    color=col,
                    marker=".",
                )
            except:
                break

        plt.xlabel("Trading age $t$")
        plt.ylabel("Optimal exit time $\\tau_E^*$")
        plt.plot(
            taueult, taueult, color="red", marker=".", label="$\\tau_E^{\\bar{*}}$"
        )
        axes = plt.gca()
        axes.set_xlim(0, teemax)
        axes.set_ylim(0, teemax)
        sm = plt.cm.ScalarMappable(
            cmap=clrmap, norm=plt.Normalize(vmin=1 - pimax, vmax=pimax)
        )
        plt.colorbar(sm, ticks=pi0value, label="Prior belief $\\pi_0$")
        plt.text(
            mar,
            1 - mar,
            str("$\\varphi  = " + str(phi) + "$"),
            horizontalalignment="left",
            verticalalignment="top",
            transform=axes.transAxes,
        )
        plt.legend(bbox_to_anchor=(1, 0), loc=2, borderaxespad=0.7, frameon=False)
        plt.tight_layout()
        plt.show()
        # plt.savefig('../figure/optc' + list(cvalue).index(c).__str__() +
        #             'phi' + list(phivalue).index(phi).__str__() + '.pdf')

    # prior belif -> ultimate exit time
    lmax = -5
    teemax = 8
    teevalue = np.linspace(start=0, stop=teemax, num=20)
    phimin = 0.01
    phimax = 1.81
    pi0value = np.concatenate(
        [
            np.linspace(start=0.9999, stop=0.9, num=10),
            np.linspace(start=0.91, stop=0.1, num=20),
            np.linspace(start=0.09, stop=0.0001, num=10),
        ],
        axis=0,
    )

    phivalue = np.linspace(start=phimin, stop=phimax, num=7)
    norm = clrs.Normalize(phimin, phimax)

    plt.close("all")
    for phi in phivalue:
        col = clrmap(norm(phi))
        taueult = []
        pimi = pi0min(phi, c=c)
        if pimi < 0.999:
            pi0value = np.arange(start=pimi + 0.00001, stop=0.996, step=0.02)
            for pi0 in pi0value:
                try:
                    taueult.extend([tauuroot(pi0, phi, c=c)])
                except:
                    next
            plt.plot(
                pi0value[0 : taueult.__len__()],
                taueult,
                color=col,
                marker=".",
                label=str(np.round(phi, 2)),
            )
        else:
            break

    plt.xlabel("Prior belief $\\pi_0$")
    plt.ylabel("Ultimate exit time $\\tau_E^{\\bar{*}}$")
    sm = plt.cm.ScalarMappable(
        cmap=clrmap, norm=plt.Normalize(vmin=phimin, vmax=phimax)
    )
    plt.colorbar(sm, ticks=phivalue, label="Running cost $\\varphi$")
    axes = plt.gca()
    axes.set_ylim([0, teemax])
    plt.tight_layout()
    plt.show()
    # plt.savefig('../figure/ultc' + list(cvalue).index(c).__str__() + '.pdf')

    colors = [[0, "limegreen"], [1, "navy"]]
    clrmap = clrs.LinearSegmentedColormap.from_list("", colors)

    plt.close("all")
    ymin = -3
    phimax = 5.8
    phimin = 0.00001
    phivalue = np.concatenate(
        [
            np.linspace(start=phimin, stop=0.24, num=999),
            np.linspace(start=0.241, stop=1.8, num=99),
            np.linspace(start=1.81, stop=phimax, num=999),
        ],
        axis=0,
    )
    teemax = 3
    teevalue = np.linspace(start=0, stop=teemax, num=5)

    phi0 = phistr(c)  # newton(lambda phi: pi0min(phi, c)-1, 1.8)
    plt.axvline(x=phi0, color="red", linestyle="--", label="$\\varphi^*$")

    norm = clrs.Normalize(0, teemax)
    for tee in teevalue:
        col = clrmap(norm(tee))
        # philim = newton(lambda phi: pimin(phi, tee, c)-1, phi0)
        phis = phivalue[phivalue < phi0]  # philim]
        plt.plot(phis, [pimin0(phi, tee, c) for phi in phis], color=col)

    plt.axvspan(phi0, phimax, alpha=0.1, color="k")
    plt.xlabel("Running cost $\\varphi$")
    plt.ylabel("Minimum prior belief $\\pi_{0*}(\\varphi, t)$")
    sm = plt.cm.ScalarMappable(cmap=clrmap, norm=plt.Normalize(vmin=0, vmax=teemax))
    plt.colorbar(sm, ticks=teevalue, label="Trading age $t$")
    # plt.legend(loc=2)
    plt.legend(bbox_to_anchor=(1, 0), loc=2, borderaxespad=0.7, frameon=False)
    axes = plt.gca()
    axes.set_xlim([0, phimax])
    axes.set_ylim([0, 1])
    plt.tight_layout()
    plt.show()
    # plt.savefig('../figure/minpic' + list(cvalue).index(c).__str__() + '.pdf')


with open("surval.pickle", "rb") as f:
    surval = pickle.load(f)

with open("surval2.pickle", "rb") as f:
    res = pickle.load(f)

for c in cvalue:
    indc = cvalue.index(c)
    for rouexit in rouexitvalue:
        indr = rouexitvalue.index(rouexit)
        for lbd in lbdvalue:
            indl = lbdvalue.index(lbd)
            plt.close("all")
            for nu in nuvalue:
                plt.plot(
                    deltats,
                    res[paramlist.index((c, lbd, rouexit, nu))],
                    label="$" + str(nu) + "$",
                )

            plt.legend(bbox_to_anchor=(1, 1), loc=2, title="$\\nu$", handlelength=0.5)
            plt.text(
                0.78,
                0.27,
                "$\lambda=" + str(lbd) + "$",
                transform=plt.gcf().transFigure,
            )
            plt.text(
                0.5,
                0.91,
                "$\\rho_{exit}=" + str(rouexit) + "$",
                transform=plt.gcf().transFigure,
            )

            plt.xlabel("$\Delta t$")
            plt.ylabel("$s(\Delta t)$")

            axes = plt.gca()
            axes.set_xlim([0, deltatsmax])
            axes.set_ylim([0, 1])
            plt.tight_layout()
            plt.show()
            # plt.savefig('../figure/survc' + str(indc) + 'r' +
            #             str(indr) + 'l' + str(indl) + '.pdf')
