from learning.funcsfinal import (
    exitpOne,
    exitrate,
    lstar,
    muGinf,
    maspOne,
    # masp,
    mur,
    murexitp,
    # murtd,
    # aggreturn,
    # ttlmas,
    murt,
    funcf,
    murtexitc,
    ttlmasOne,
)
from learning.constants import teev, res, rouexit, lambdalg, lambdalb, ns, Ts, FIG_DIR
import pandas as pd
from time import time
import matplotlib as mpl
import matplotlib.pyplot as plt
import pickle
import numpy as np
from scipy.integrate import quad
from numba import njit

fv = np.vectorize(funcf)(teev)

# plt.plot(teev, fv)
# plt.show()

# \mu_inf
mugv = np.vectorize(muGinf)(teev)
# \mu_G
masptv = np.vectorize(maspOne)(teev, s=True)
# \mu_B
maspfv = np.vectorize(maspOne)(teev, s=False)

maspv = masptv + maspfv
# check, same as np.vectorize(masp)(teev)
masv = maspv + mugv

# calt = 10
# rets = quad(lambda tee: (muGinf(tee) + maspOne(tee)) * funcf(tee),


exit_rate_G = -(rouexit + fv) * masptv - np.vectorize(exitrate)(teev, s=True)
exit_rate_B = -rouexit * maspfv - np.vectorize(exitrate)(teev, s=False)

exit_rate_inf = -rouexit * mugv + fv * masptv


exitptv = np.vectorize(exitpOne)(teev, s=True)
exitpfv = np.vectorize(exitpOne)(teev, s=False)
exitpv = exitptv + exitpfv
# check, same as np.vectorize(exitp)(teev)
exitmasv = (1 - rouexit) * exitpv + rouexit * masv
# tapv = np.vectorize(funcetap)(teev)

aggreturnptv = masptv * fv
aggreturnpv = mugv * fv + aggreturnptv
# check, same as np.vectorize(aggreturn)(teev)

dtatemp = pd.DataFrame()
for i in range(len(res)):
    ret = res[i]
    dtatemp["R" + str(i)] = np.vectorize(mur)(R=ret, tee=teev)

    # \mu^R_{exit}(R, t)
    temp = np.vectorize(murexitp)(R=ret, tee=teev)
    dtatemp["exr" + str(i)] = (1 - rouexit) * temp + rouexit * dtatemp["R" + str(i)]


# average return by age
yyall = aggreturnpv / masv
# average return by age, good type
yyg = fv
# average return by age, bad type
yyb = [0] * len(teev)



plt.plot(
    teev,
    yyall,
    "k",
    label="Average return, aggregate: $\\frac{\\int R\\mu^R(R, t) dR}{m(t)}$",
)
plt.plot(teev, yyg, "--", label="Average return, type $G$: $f(t)$")
plt.plot(teev, yyb, "-.", label="Average return, type $B$: 0")
plt.xlabel("Trading age $t$")
plt.ylabel("Average return")
axes = plt.gca()
axes.set_ylim(-0.01, 0.99)
axes.spines["right"].set_visible(False)
axes.spines["top"].set_visible(False)
axes.xaxis.set_ticks_position("bottom")
axes.yaxis.set_ticks_position("left")

plt.legend(bbox_to_anchor=(1, 0), loc=4, handlelength=2, frameon=False)

plt.tight_layout()
plt.savefig(FIG_DIR / "AvgReturn.pdf")
plt.show()


plt.close()

plt.plot(teev, (mugv + masptv) / masv)
plt.show()

plt.close()
for teevind in [49, 50, 51]:
    i = ((mugv + masptv) / masv)[teevind]
    temptee = teev[teevind]
    sel = np.where(teev <= temptee)[0]
    plt.plot(
        teev[sel],
        (i * yyg)[sel],
        label="$\\frac{\\mu_G(t ="
        + str(temptee)
        + ")+\\mu_{inf}(t ="
        + str(temptee)
        + ")}{m(t ="
        + str(temptee)
        + ")} = $"
        + str(round(i, 2)),
    )
plt.xlabel("Trading age $t$")
plt.ylabel("Average return")
axes = plt.gca()
# axes.set_ylim([0.38, 0.79])
axes.spines["right"].set_visible(False)
axes.spines["top"].set_visible(False)
axes.xaxis.set_ticks_position("bottom")
axes.yaxis.set_ticks_position("left")

plt.legend(
    loc="lower center", handlelength=2, frameon=False, title="Proportion of type $G$"
)

plt.tight_layout()
plt.savefig(FIG_DIR / "CohortReturn.pdf")
plt.show()


# mu_exit(t,R)
tempdta = dtatemp.iloc[:, dtatemp.columns.str.startswith("exr")]
# mass(t,R)
tempdtaR = dtatemp.iloc[:, dtatemp.columns.str.startswith("R")]


# return mass plot at different age levels
plt.close("all")
for i in [6, 50, 64]:
    tv = teev[i]
    plt.plot(res, np.array(tempdtaR[i : (i + 1)])[0], label="$t=" + str(tv) + "$")
axes = plt.gca()
# axes.set_xlim([-1.99, 1.99])
# axes.set_ylim([0, 0.51])
axes.spines["right"].set_visible(False)
axes.spines["top"].set_visible(False)
axes.xaxis.set_ticks_position("bottom")
axes.yaxis.set_ticks_position("left")

plt.legend(
    bbox_to_anchor=(1, 1), loc=1, title="Trading age $t$", handlelength=1, frameon=False
)
plt.xlabel("Return $R$")
plt.ylabel("Trading mass $\\frac{\\mu^R(R, t)}{M(t)}$")
plt.tight_layout()
plt.savefig(FIG_DIR / "MassReturn.pdf")
plt.show()


# exit mass-return plot
plt.close("all")
tempdta = dtatemp.iloc[:, dtatemp.columns.str.startswith("exr")]
for i in [6, 50, 64]:
    tv = teev[i]
    plt.plot(res, np.array(tempdta[i : (i + 1)])[0], label="$t=" + str(tv) + "$")
axes = plt.gca()
# axes.set_xlim([-1.99, 1.99])
plt.legend(bbox_to_anchor=(1, 1), loc=1, title="Trading age $t$", handlelength=1)
plt.xlabel("Return $R$")
plt.ylabel("$\\mu_{exit}^R(R,t)$")
plt.tight_layout()
plt.show()
# plt.savefig('../figure/MassReturnExit.pdf')


# return density plot at different age levels
plt.close("all")
for i in [6, 50, 64]:
    tv = teev[i]
    plt.plot(
        res, np.array(tempdtaR[i : (i + 1)] / masv[i])[0], label="$t=" + str(tv) + "$"
    )
axes = plt.gca()
# axes.set_xlim([-1.99, 1.99])
# axes.set_ylim([0, 0.51])
axes.spines["right"].set_visible(False)
axes.spines["top"].set_visible(False)
axes.xaxis.set_ticks_position("bottom")
axes.yaxis.set_ticks_position("left")

plt.legend(
    bbox_to_anchor=(1, 1), loc=1, title="Trading age $t$", handlelength=1, frameon=False
)
plt.xlabel("Return $R$")
plt.ylabel("Density $\\frac{\\mu^R(R, t)}{m(t)}$")
plt.tight_layout()
plt.savefig(FIG_DIR / "MassReturn.pdf")
plt.show()


# plot mu_exit(t,R)/mass(t,R) -- unique prediction, procrastination

plt.close("all")
for i in [40, 50, 64]:
    tv = teev[i]
    plt.plot(
        res,
        (np.array(tempdta[i : (i + 1)])[0] / np.array(tempdtaR[i : (i + 1)])[0]),
        label="$t=" + str(tv) + "$",
    )
axes = plt.gca()
axes.set_ylim(0, 0.134)
axes.spines["right"].set_visible(False)
axes.spines["top"].set_visible(False)
axes.xaxis.set_ticks_position("bottom")
axes.yaxis.set_ticks_position("left")

plt.legend(
    bbox_to_anchor=(1, 1), loc=1, title="Trading age $t$", handlelength=1, frameon=False
)
plt.xlabel("Return $R$")
plt.ylabel("Exit likelihood $\\frac{\\mu_{exit}^R(R, t)}{\\mu^R(R, t)}$")
plt.tight_layout()
plt.savefig(FIG_DIR / "ExitLikeliGivenAgePy.pdf")
plt.show()


plt.close("all")
for i in [0, 20, 40]:
    plt.plot(
        teev,
        tempdta["exr" + str(i)] / tempdtaR["R" + str(i)],
        label="$R=" + str(res[i]) + "$",
    )
axes = plt.gca()
axes.set_ylim(0, 0.134)
axes.spines["right"].set_visible(False)
axes.spines["top"].set_visible(False)
axes.xaxis.set_ticks_position("bottom")
axes.yaxis.set_ticks_position("left")

plt.legend(
    bbox_to_anchor=(1, 1),
    loc=1,
    title="Trading return $R$",
    handlelength=1,
    frameon=False,
)
plt.xlabel("Trading age $t$")
plt.ylabel("Exit likelihood $\\frac{\\mu_{exit}^R(R, t)}{\\mu^R(R, t)}$")
plt.tight_layout()
plt.savefig(FIG_DIR / "ExitLikeliGivenReturnPy.pdf")
plt.show()


# exit: return density at different age
plt.close("all")
for i in [6, 50, 64]:
    tv = teev[i]
    plt.plot(
        res,
        np.array(tempdta[i : (i + 1)] / exitmasv[i])[0],
        label="$t=" + str(tv) + "$",
    )
axes = plt.gca()
# axes.set_xlim([-1.99, 1.99])
# axes.set_ylim([0, 0.41])
axes.spines["right"].set_visible(False)
axes.spines["top"].set_visible(False)
axes.xaxis.set_ticks_position("bottom")
axes.yaxis.set_ticks_position("left")

plt.legend(
    bbox_to_anchor=(1, 1), loc=1, title="Trading age $t$", handlelength=1, frameon=False
)
plt.xlabel("Return $R$")
plt.ylabel("$\\frac{\\mu_{exit}^R(R,t)}{exit(t)}$")
plt.tight_layout()
plt.show()
# plt.savefig('../figure/DensReturnExit.pdf')


# compare remain - exit
plt.close("all")
for i in [6, 50, 64]:
    tv = teev[i]
    plt.plot(
        res,
        (np.array(tempdtaR[i : (i + 1)])[0]) / masv[i],
        label="Remain density $\\frac{\\mu^R(R, t="
        + str(tv)
        + ")}{m(t="
        + str(tv)
        + ")}$",
    )
    plt.plot(
        res,
        np.array(tempdta[i : (i + 1)])[0] / exitmasv[i],
        label="Exit density $\\frac{\\mu_{exit}^R(R, t="
        + str(tv)
        + ")}{m_{exit}(t="
        + str(tv)
        + ")}$",
    )
    axes = plt.gca()
    # axes.set_xlim([-0.45, 0.88])
    axes.set_ylim(0, 0.44)
    axes.spines["right"].set_visible(False)
    axes.spines["top"].set_visible(False)
    axes.xaxis.set_ticks_position("bottom")
    axes.yaxis.set_ticks_position("left")

    plt.legend(loc="upper left", handlelength=1, frameon=False)
    plt.xlabel("Return $R$")
    plt.ylabel("Density")
    plt.tight_layout()
    plt.show()
    # plt.savefig('../figure/ExitRemainReturn' + str(tv) + '.pdf')


# exit: age density at different R
for k in ns:
    plt.close("all")
    sel = np.where(teev < Ts[k])[0]
    for i in [0, 20, 40]:
        murexitt = (1 - rouexit) * murtexitc(R=res[i], Time=Ts[k]) + rouexit * murt(
            Time=Ts[k], R=res[i]
        )
        plt.plot(
            teev[sel],
            tempdta.iloc[sel, i] / murexitt,
            label="$R=" + res[i].round(2).__str__() + "$",
        )
    axes = plt.gca()
    # axes.set_xlim([0, 6])
    # axes.set_ylim([0, 0.99])
    axes.spines["right"].set_visible(False)
    axes.spines["top"].set_visible(False)
    axes.xaxis.set_ticks_position("bottom")
    axes.yaxis.set_ticks_position("left")

    plt.legend(
        bbox_to_anchor=(1, 1), loc=1, title="Return $R$", handlelength=1, frameon=False
    )
    plt.xlabel("Trading age $t$")
    plt.ylabel("Density $\\frac{\\mu_{exit}^R(R,t)}{\\int_0^T \\mu_{exit}^R(R, t) dt}$")
    plt.tight_layout()
    # plt.savefig('../figure/exitgivenRT' + str(k) + '.pdf')
    plt.show()


#
# exit_rate_G
# exit_rate_B
#
# exit_rate_inf

plt.close()
plt.plot(teev, exit_rate_G + exit_rate_B + exit_rate_inf, "k", label="Total")
plt.plot(teev, exit_rate_inf, ":", label="Type $inf$")
plt.plot(teev, exit_rate_G, "--", label="Type $G$")
plt.plot(teev, exit_rate_B, "-.", label="Type $B$")
axes = plt.gca()
axes.set_ylim(-0.25, 0.25)
axes.set_xlim(0, 17.6)
axes.spines["right"].set_visible(False)
axes.spines["top"].set_visible(False)
axes.xaxis.set_ticks_position("bottom")
axes.yaxis.set_ticks_position("left")

plt.legend(bbox_to_anchor=(1, 1), loc=1, handlelength=2, frameon=False)

plt.xlabel("Trading age $t$")
plt.ylabel("Evolution of mass")
plt.tight_layout()
plt.savefig(FIG_DIR / "ExitMasPy.pdf")
plt.show()


plt.close()
plt.plot(
    teev,
    -(exit_rate_G + exit_rate_B + exit_rate_inf) / masv,
    "k",
    label="Total exit $-\\frac{m'(t)}{m(t)}$",
)
# plt.plot(teev,  [rouexit] * len(teev), ':', label='Exit due to exogenous shock')
plt.plot(
    teev,
    -(exit_rate_G + exit_rate_inf) / masv,
    "--",
    label="Exit from type $G$ and $inf$",
)
plt.plot(teev, -exit_rate_B / masv, "-.", label="Exit from type $B$")
plt.xlabel("Trading age $t$")
plt.ylabel("Exit likelihood")
axes = plt.gca()
axes.set_ylim(0, 0.129)
axes.set_xlim(0, 17.6)
axes.spines["right"].set_visible(False)
axes.spines["top"].set_visible(False)
axes.xaxis.set_ticks_position("bottom")
axes.yaxis.set_ticks_position("left")

plt.legend(bbox_to_anchor=(1, 1), loc=1, handlelength=2, frameon=False)

plt.tight_layout()
plt.savefig(FIG_DIR /"ExitlikeliPy.pdf")
plt.show()


# deprecated
# plt.close()
# plt.plot(teev, exitmasv, 'k', label='Total exit $exit(t)$')
# plt.plot(teev, rouexit * masv, ':', label='Exit due to exogenous shock')
# plt.plot(teev, (1-rouexit)*exitptv, '--', label='Exit from type $G$')
# plt.plot(teev, (1-rouexit)*exitpfv, '-.', label='Exit from type $B$')
# axes = plt.gca()
# axes.set_ylim([0, 0.109])
# axes.set_xlim([0, 17.6])
# axes.spines['right'].set_visible(False)
# axes.spines['top'].set_visible(False)
# axes.xaxis.set_ticks_position('bottom')
# axes.yaxis.set_ticks_position('left')
#
# plt.legend(bbox_to_anchor=(1, 1), loc=1,
#                handlelength=2, frameon=False)
#
# plt.xlabel("Trading age $t$")
# plt.ylabel("Mass of exiting traders")
# plt.tight_layout()
# plt.savefig(FIG_DIR + 'ExitMasPy.pdf')
# plt.show()
#
#
#
# # concave -- which is also intuitive, goes up first because immortal type increases,
# # then goes down because ultimately people will all be gone due to exogenous shock
# plt.close()
# plt.plot(teev, exitmasv/masv, 'k', label='Total exit $exit(t)$')
# plt.plot(teev,  [rouexit] * len(teev), ':', label='Exit due to exogenous shock')
# plt.plot(teev, (1 - rouexit) * exitptv/masv, '--', label='Exit from type $G$')
# plt.plot(teev, (1 - rouexit) * exitpfv/masv, '-.', label='Exit from type $B$')
# plt.xlabel("Trading age $t$")
# plt.ylabel("Fraction of exiting mass in total mass $\\frac{exit(t)}{m(t)}$")
# axes = plt.gca()
# axes.set_ylim([0, 0.129])
# axes.set_xlim([0, 17.6])
# axes.spines['right'].set_visible(False)
# axes.spines['top'].set_visible(False)
# axes.xaxis.set_ticks_position('bottom')
# axes.yaxis.set_ticks_position('left')
#
# plt.legend(bbox_to_anchor=(1, 1), loc=1,
#                handlelength=2, frameon=False)
#
# plt.tight_layout()
# plt.savefig(FIG_DIR + 'ExitlikeliPy.pdf')
# plt.show()


plt.close("all")
for i in [6, 50, 64]:
    tv = teev[i]
    plt.plot(res, np.array(tempdtaR[i : (i + 1)])[0], label="Remain")
    plt.plot(res, np.array(tempdta[i : (i + 1)])[0], label="Exit")
    axes = plt.gca()
    # axes.set_xlim([-1.99, 1.99])
    axes.set_ylim(0, 0.42)
    plt.legend(
        bbox_to_anchor=(1, 1),
        loc=1,
        title="Trading age $t=" + str(tv) + "$",
        handlelength=1,
    )
    plt.xlabel("Return $R$")
    plt.ylabel("Mass")
    plt.show()


# plot of l_*(t)
plt.close()
for phiv in [0.01, 0.05, 0.35]:
    plt.plot(
        teev,
        [lstar(tee=i, phi=phiv) for i in teev],
        label="$\\varphi=" + str(phiv) + "$",
    )
axes = plt.gca()
axes.spines["right"].set_visible(False)
axes.spines["top"].set_visible(False)
axes.xaxis.set_ticks_position("bottom")
axes.yaxis.set_ticks_position("left")
# axes.set_xlim([0, 9.99])

plt.xlabel("Trading age $t$")
plt.ylabel("Exit belief boundary $\\ell_*(t, \\varphi)$")
plt.legend(
    bbox_to_anchor=(1, 1),
    loc=1,
    title="Running cost $\\varphi$",
    handlelength=0.5,
    frameon=False,
)
# plt.savefig('../figure/lstar.pdf')
plt.show()


# Mass through time
plt.close()
Tvs = teev
# [0.001, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5, 3, 4, 5, 6, 8, 10, 12, 15, 20]

massinf = np.vectorize(
    lambda Time: quad(lambda x: muGinf(x), 0, Time, epsrel=0.001)[0]
)(Tvs)

masst = np.vectorize(ttlmasOne)(Tvs, s=True)
massf = np.vectorize(ttlmasOne)(Tvs, s=False)
masss = masst + massinf + massf
# masss = np.vectorize(ttlmas)(Tvs)
# with open('masss3.pickle', 'wb') as f:
#     pickle.dump(masss, f)
#
# with open('masss3.pickle', 'rb') as f:
#     masss = pickle.load(f)


plt.plot(Tvs, masss, "k", label="Total mass $M(t)$")
plt.plot(Tvs, massinf, ":", label="$inf$ mass")
plt.plot(Tvs, masst, "--", label="Type $G$ mass (excl. $inf$)")
plt.plot(Tvs, massf, "-.", label="Type $B$ mass")
axes = plt.gca()
axes.set_xlim(0, 19.9)
axes.set_ylim(0, 14.9)
axes.spines["right"].set_visible(False)
axes.spines["top"].set_visible(False)
axes.xaxis.set_ticks_position("bottom")
axes.yaxis.set_ticks_position("left")
plt.legend(bbox_to_anchor=(0, 1), loc=2, handlelength=2, frameon=False)

plt.xlabel("Calendar time $T$ (Platform age)")
plt.ylabel("Trading mass")
plt.tight_layout()
plt.savefig(FIG_DIR / "MassThruTimePy.pdf")
plt.show()


aggr = np.vectorize(
    lambda Time: quad(
        lambda x: (muGinf(x) + maspOne(x, True)) * funcf(x), 0, Time, epsrel=0.001
    )[0]
)(Tvs)

plt.plot(Tvs, aggr / masss, "k")
axes = plt.gca()
axes.set_xlim(0, 19.9)
axes.set_ylim(0, 0.54)
axes.spines["right"].set_visible(False)
axes.spines["top"].set_visible(False)
axes.xaxis.set_ticks_position("bottom")
axes.yaxis.set_ticks_position("left")

plt.xlabel("Calendar time $T$ (Platform age)")
plt.ylabel("Average return across entire platform")
plt.tight_layout()
plt.savefig(FIG_DIR / "AvgRThruTimePy.pdf")
plt.show()


aggexit = np.vectorize(
    lambda Time: quad(
        lambda x: exitpOne(x, True) + exitpOne(x, False), 0, Time, epsrel=0.001
    )[0]
)(Tvs)


plt.plot(Tvs, rouexit + (1 - rouexit) * aggexit / masss, "k")
axes = plt.gca()
axes.set_xlim(0, 19.9)
axes.set_ylim(0, 0.042)
axes.spines["right"].set_visible(False)
axes.spines["top"].set_visible(False)
axes.xaxis.set_ticks_position("bottom")
axes.yaxis.set_ticks_position("left")

plt.xlabel("Calendar time $T$ (Platform age)")
plt.ylabel("Aggregate exit likelihood")
plt.tight_layout()
plt.savefig(FIG_DIR / "ExitThruTimePy.pdf")
plt.show()


plt.plot(Tvs, rouexit * masss + (1 - rouexit) * aggexit, "k")
axes = plt.gca()
axes.set_xlim(0, 19.9)
# axes.set_ylim([0, 0.042])
axes.spines["right"].set_visible(False)
axes.spines["top"].set_visible(False)
axes.xaxis.set_ticks_position("bottom")
axes.yaxis.set_ticks_position("left")

plt.xlabel("Calendar time $T$ (Platform age)")
plt.ylabel("Aggregate exit mass")
plt.tight_layout()
plt.savefig(FIG_DIR / "ExitMassThruTimePy.pdf")
plt.show()

plt.close()
fig, ax1 = plt.subplots()
ax1.set_xlim([0, 19.9])
ax1.set_ylim([0, 0.042])
ax1.plot(Tvs, rouexit + (1 - rouexit) * aggexit / masss, "k")
ax1.set_xlabel("Calendar time $T$ (Platform age)")
ax1.set_ylabel("Aggregate exit likelihood")

ax2 = ax1.twinx()
color = "tab:red"
ax2.set_ylabel("sin", color=color)  # we already handled the x-label with ax1
ax2.set_ylim([0, 0.54])
ax2.plot(Tvs, aggr / masss, color=color)
ax2.tick_params(axis="y", labelcolor=color)
ax2.set_ylabel("Average return across entire platform", color=color)
plt.tight_layout()
plt.savefig(FIG_DIR / "returnexitPy.pdf")
plt.show()


# Trading mass by trading age
plt.close()
plt.plot(teev, masv, "k", label="Total mass $m(t)$")
plt.plot(teev, mugv, ":", label="$inf$ mass $\mu_{inf}(t)$")
plt.plot(teev, masptv, "--", label="Type $G$ mass (excl. $inf$)")
plt.plot(teev, maspfv, "-.", label="Type $B$ mass")
axes = plt.gca()
axes.set_xlim(0, 19.9)
axes.set_ylim(0, 1)
plt.xlabel("Trading age $t$")
plt.ylabel("Trading mass")
axes.spines["right"].set_visible(False)
axes.spines["top"].set_visible(False)
axes.xaxis.set_ticks_position("bottom")
axes.yaxis.set_ticks_position("left")

plt.legend(bbox_to_anchor=(1, 1), loc=1, handlelength=2, frameon=False)
plt.tight_layout()
plt.savefig(FIG_DIR / "MassByAgePy.pdf")
plt.show()


plt.close()
for i in [56, 60, 64]:
    Tv = Tvs[i]
    sel = teev <= Tv
    plt.plot(teev[sel], masv[sel] / masss[i], label=Tv)
axes = plt.gca()
# axes.set_xlim([2, 8])
# axes.set_ylim([0, 1e-7])
plt.xlabel("Trading age $t$")
plt.ylabel("Density $\\frac{m(t)}{M(T)}, t \\leq T$")
axes.spines["right"].set_visible(False)
axes.spines["top"].set_visible(False)
axes.xaxis.set_ticks_position("bottom")
axes.yaxis.set_ticks_position("left")

plt.legend(
    bbox_to_anchor=(1, 1),
    loc=1,
    title="Calendar time $T$",
    handlelength=1,
    frameon=False,
)
plt.tight_layout()
plt.savefig(FIG_DIR / "AgeDensityGivenTimePy.pdf")
plt.show()


# mass-return plot
plt.close("all")
for k in ns:
    sel = np.where(teev < Ts[k])[0]
    for i in [0, 20, 40]:
        plt.plot(
            teev[sel],
            tempdtaR.iloc[sel, i] / murt(Time=Ts[k], R=res[i]),
            label="$R=" + res[i].round(2).__str__() + "$",
        )
    axes = plt.gca()
    # axes.set_xlim([0, 6])
    # axes.set_ylim([0, 0.99])
    plt.legend(bbox_to_anchor=(1, 1), loc=1, title="Return $R$", handlelength=1)
    plt.xlabel("Trading age $t$")
    plt.ylabel("Density $\\frac{\\mu^R(R, t)}{\\int_0^T \\mu^R(R, t) dt}$")
    plt.show()


# plt.savefig('../figure/totalmass.pdf')

# mpl.use("pgf")
#
# mpl.rcParams.update({                      # setup matplotlib to use latex for output
#     "pgf.texsystem": "pdflatex",        # change this if using xetex or lautex
#     "text.usetex": True,                # use LaTeX to write all text
#     "axes.labelsize": 15,
#     "font.size": 15,
#     "legend.fontsize": 15,               # Make the legend/label fonts
#     "xtick.labelsize": 15,               # a little smaller
#     "ytick.labelsize": 15,
#     "pgf.preamble": [
#         r'\usepackage{amsmath,amsfonts,amssymb}',
#         ]
#     })


mpl.rcParams.update({"legend.fontsize": 10})


@njit
def nul(l: float, phi: float, skills: bool):
    x = lstar(tee=0, phi=phi) - l
    return np.exp(lambdal(skills) / x) / (x * x)


@njit
def lambdal(skills: bool):
    return np.where(skills, lambdalg, lambdalb).item()


lambdalg = 30  # 0.7 murtdta3 with "* l" not "/ l"

# lambda of \ell distribution for good traders
lambdalb = 20  # 0.8 murtdta3 "* l" not "/ l"

phiv = 0.0001
plt.close()
ls = np.linspace(lstar(tee=0, phi=phiv) + 1e-9, 80, 5000)
plt.plot(ls, [lambdal(True) * nul(i, phiv, True) for i in ls], label="Type $G$")
plt.plot(ls, [lambdal(False) * nul(i, phiv, False) for i in ls], label="Type $B$")
axes = plt.gca()
# axes.set_xlim([-19.9, 79.9])
# axes.set_ylim([0, 11.1])
axes.spines["right"].set_visible(False)
axes.spines["top"].set_visible(False)
axes.xaxis.set_ticks_position("bottom")
axes.yaxis.set_ticks_position("left")
plt.xlabel("Trading age $t$")
plt.ylabel("Belief density")
plt.legend(bbox_to_anchor=(1, 1), loc=1, handlelength=0.5, frameon=False)
plt.tight_layout()
plt.savefig(FIG_DIR / "ldist1.pdf")
plt.show()

phiv = 0.2
plt.close()
ls = np.linspace(lstar(tee=0, phi=phiv) + 1e-9, 80, 5000)
plt.plot(ls, [1 * nul(i, phiv, True) for i in ls], label="Type $G$")
plt.plot(ls, [0.2 * nul(i, phiv, False) for i in ls], label="Type $B$")
axes = plt.gca()
# axes.set_xlim([-19.9, 79.9])
# axes.set_ylim([0, 11.1])
axes.spines["right"].set_visible(False)
axes.spines["top"].set_visible(False)
axes.xaxis.set_ticks_position("bottom")
axes.yaxis.set_ticks_position("left")
plt.xlabel("Trading age $t$")
plt.ylabel("Belief density")
plt.legend(bbox_to_anchor=(1, 1), loc=1, handlelength=0.5, frameon=False)
plt.tight_layout()
plt.savefig(FIG_DIR / "ldist2.pdf")
plt.show()


#
# rs = np.linspace(-10, 10, 900)
# def tmp(k):
#     return k*norm.pdf(rs, 1, 0.9) + (1-k) * norm.pdf(rs, 0, 1)
#
# plt.plot(rs, tmp(0.4)/tmp(0.8))
# plt.show()
#
# plt.plot(rs, tmp(0.4))
# plt.plot(rs, tmp(0.8))
# plt.show()
