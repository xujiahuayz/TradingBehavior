from learning.funcsfinal import (
    exitpOne,
    exitrate,
    muGinf,
    maspOne,
    mur,
    murexitp,
    funcf,
    ttlmasOne,
)
from learning.constants import teev, res, rouexit,  FIG_DIR
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from scipy.integrate import quad

fv = np.vectorize(funcf)(teev)

# \mu_inf
mugv = np.vectorize(muGinf)(teev)
# \mu_G
masptv = np.vectorize(maspOne)(teev, s=True)
# \mu_B
maspfv = np.vectorize(maspOne)(teev, s=False)

maspv = masptv + maspfv
# check, same as np.vectorize(masp)(teev)
masv = maspv + mugv


exit_rate_G = -(rouexit + fv) * masptv - np.vectorize(exitrate)(teev, s=True)
exit_rate_B = -rouexit * maspfv - np.vectorize(exitrate)(teev, s=False)

exit_rate_inf = -rouexit * mugv + fv * masptv


exitptv = np.vectorize(exitpOne)(teev, s=True)
exitpfv = np.vectorize(exitpOne)(teev, s=False)
exitpv = exitptv + exitpfv
# check, same as np.vectorize(exitp)(teev)
exitmasv = (1 - rouexit) * exitpv + rouexit * masv

aggreturnptv = masptv * fv
aggreturnpv = mugv * fv + aggreturnptv
# check, same as np.vectorize(aggreturn)(teev)

dtatemp = pd.DataFrame()
for i in range(len(res)):
    ret = res[i]
    dtatemp["R" + str(i)] = np.vectorize(mur)(R=ret, tee=teev)

    temp = np.vectorize(murexitp)(R=ret, tee=teev)
    dtatemp["exr" + str(i)] = (1 - rouexit) * temp + rouexit * dtatemp["R" + str(i)]


# average return by age
yyall = aggreturnpv / masv
# average return by age, good type
yyg = fv
# average return by age, bad type
yyb = [0] * len(teev)


## Fig 3
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


## Fig 4
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



## Fig 2(a)
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


## Fig 2(b)
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

## Fig 1(b)
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


## Fig 1(a)
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
