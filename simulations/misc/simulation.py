# for name in dir():
#     del globals()[name]

from scipy.optimize import brentq, newton
import matplotlib as mpl
import matplotlib.pyplot as plt
from funcs import *
import matplotlib.colors as clrs

# xrange = range(9999)
# plt.plot(xrange, [fint(t) for t in xrange], color='blue', label='$\\tau_E^{\\bar{*}}$')
# plt.plot(xrange, [funcf(t) for t in xrange], color='red', label='$\\tau_E^{\\bar{*}}$')
# # plt.contour(
# #     np.outer(np.linspace(1, 1, 30),tees), np.outer(cnts,np.linspace(1, 1, 999)),
# #     data, cmap = clrmap, levels = np.array([0.45, 0.5, 0.65]))
# plt.show()


mpl.use("pgf")

pgf_with_latex = {                      # setup matplotlib to use latex for output
    "pgf.texsystem": "pdflatex",        # change this if using xetex or lautex
    "text.usetex": True,                # use LaTeX to write all text
    "axes.labelsize": 20,
    "font.size": 20,
    "legend.fontsize": 20,               # Make the legend/label fonts
    "xtick.labelsize": 20,               # a little smaller
    "ytick.labelsize": 20,
    "pgf.preamble": [
        # r"\usepackage[utf8]{inputenc}",    # use utf8 input and T1 fonts
        # r"\usepackage[T1]{fontenc}",
        r'\usepackage{amsmath,amsfonts,amssymb}',
        # r'\usepackage[scientific-notation=true]{siunitx}',
        ]
    }

mpl.rcParams.update(pgf_with_latex)

# mpl.use("module://backend_interagg")
# mpl.rcdefaults()


teemax = 60
tees = range(teemax)

plt.close('all')
plt.plot(tees, [funcf(t) for t in tees], color='darkgrey')
plt.xlabel("Trading age $t$")
plt.ylabel("Intensity of success arrival $f(t)$")
axes = plt.gca()
axes.set_xlim([0, teemax])
axes.set_ylim([0, 2])
plt.tight_layout()
# plt.show()
plt.savefig('../figure/funcf' + '.pdf')

colors = [[0, "white"],
          [0.2, "goldenrod"],
          [0.4, "darkgoldenrod"],
          [0.5, "black"],
          [0.6, "darkgreen"],
          [0.8, "green"],
          [1, "white"]]
clrmap = clrs.LinearSegmentedColormap.from_list("", colors)

cnts = range(60)

teemax = 45
tees = np.linspace(start=0, stop=teemax, num=99)
data = np.array([[pmf(tee, n) for tee in tees] for n in cnts])

plt.close('all')
plt.pcolormesh(tees, cnts, data, cmap = clrmap)
plt.plot(tees, [fint(t) for t in tees], color='red', label='$\\mathbb{E}[N_t]$')

plt.xlabel("Trading age $t$")
plt.ylabel("Number of successes $n$")

axes = plt.gca()
axes.set_ylim([0, np.max(cnts)])

plt.colorbar(label='$\\mathbb{P}[N_t < n]$')
plt.legend(bbox_to_anchor=(1, 0), loc = 2, borderaxespad=0.7)

plt.tight_layout()
# plt.show()
plt.savefig('../figure/probn' + '.pdf')

tmean = []
ptmean = []
cnts = [1, 3, 10, 20, 30]

plt.close('all')
for n in cnts:
    plt.plot(tees, [1 - pmf(t, n) for t in tees], label=str(n))
    temp = tmeans(n)
    tmean.extend([temp])
    ptmean.extend([1-pmf(temp, n)])

leg1 = plt.legend(bbox_to_anchor=(1, 1), loc=2, title='$n$')
plt.gca().add_artist(leg1)

sct = plt.scatter(tmean, ptmean, marker='D', color = 'black',
            label='$\\mathbb{E}[t\\,|\\,N_t=n]$')
plt.legend([sct], ['$\\mathbb{E}[t\\,|\\,N_t=n]$'],
           bbox_to_anchor=(1, 0), loc = 3)
plt.xlabel("Trading age $t$")
plt.ylabel("$\\mathbb{P}[\\tau \leq t \\,|\\, N_{\\tau} = n]$")

axes = plt.gca()
axes.set_xlim([0, 30])
axes.set_ylim([0, 1])
plt.tight_layout()
# plt.show()
plt.savefig('../figure/cdft' + '.pdf')


# ls = np.linspace(start=-10, stop=10, num=21)
# plt.scatter(ls, funcpi(ls))
# plt.xlabel("ell")
# plt.ylabel("pi(ell)")
# plt.show()

mar = 0.03

phimax = 1.21
phivalue = np.linspace(start=0.01, stop=phimax, num = 3)
phi = phivalue[1]
teemax = 1.8
teevalue = np.linspace(start=0, stop=teemax, num=7)

taumax = 3
tauetry = np.linspace(start=0, stop=taumax, num=50)
l0value = np.linspace(start=-0.5, stop=0.5, num=3)

colors = [[0, "lightgrey"],
          [1, "black"]]
clrmap = clrs.LinearSegmentedColormap.from_list("", colors)

norm = clrs.Normalize(0, teemax)
# exit -> value through time
for l0 in l0value:
    plt.close('all')
    ves = []
    vopt = []
    eopt = []
    for tee in teevalue:
        col = clrmap(norm(tee))
        taus = tauetry[tauetry > tee]
        vestemp = [ve(l0, tee, phi, taue) for taue in taus]
        ves.append(vestemp)

        maxv = max(vestemp)
        vopt.extend([maxv])
        eopt.extend([taus[vestemp.index(maxv)]])

        plt.plot(taus, vestemp, color=col, marker='o')
        plt.xlabel("Exit time $\\tau_E$")
        plt.ylabel("Expected value $\\mathbb{E}[V]$")

    plt.plot(eopt, vopt, color='red', marker='o', label = '$\\tau_E^*$')
    axes = plt.gca()
    axes.set_xlim([0, taumax])
    axes.set_ylim([0, 5])
    sm = plt.cm.ScalarMappable(cmap=clrmap, norm=plt.Normalize(vmin=0, vmax=teemax))
    plt.colorbar(sm, ticks = teevalue, label='Trading age $t$')
    plt.text(mar, 1-mar, str('$\\ell_0 = ' + str(l0) + '$'),
             horizontalalignment='left', verticalalignment='top', transform=axes.transAxes)
    plt.legend(bbox_to_anchor=(1, 0), loc = 2, borderaxespad=0.7)
    plt.tight_layout()
    # plt.show()
    plt.savefig('../figure/exi' + list(l0value).index(l0).__str__() + '.pdf')


lmax = 3.6
norm = clrs.Normalize(-lmax, lmax)
# time -> optimal exit tim
teemax = 8
teevalue = np.linspace(start=0, stop=teemax, num=40)
l0value = np.linspace(start=lmax, stop=-lmax, num=7)
# exit -> value through time
for phi in phivalue:
    plt.close('all')
    taueult = []
    for l0 in l0value:
        col = clrmap(norm(l0))
        taueopt = []
        for tee in teevalue:
            try:
                taueopt.extend([brentq(lambda taue: veprime(l0, tee, phi, taue), tee, 30)])
            except:
                taueopt.extend([tee])
                taueult.extend([tee])
                break

        plt.plot(teevalue[0:taueopt.__len__()], taueopt, color=col, marker='o'
                 # , label = '$' + str(np.round(l0,2)) + '$'
                 )

    plt.xlabel("Trading age $t$")
    plt.ylabel("Optimal exit time $\\tau_E^*$")
    plt.plot(taueult, taueult, color='red', marker='o', label = '$\\tau_E^{\\bar{*}}$')
    axes = plt.gca()
    axes.set_xlim([0, teemax])
    axes.set_ylim([0, teemax])
    sm = plt.cm.ScalarMappable(cmap=clrmap, norm=plt.Normalize(vmin=-lmax, vmax=lmax))
    plt.colorbar(sm, ticks=l0value, label='Prior belief $\\ell_0$')
    plt.text(mar, 1-mar, str('$\\varphi  = ' + str(phi) + '$'),
             horizontalalignment='left',verticalalignment='top', transform=axes.transAxes)
    plt.legend(bbox_to_anchor=(1, 0), loc = 2, borderaxespad=0.7)

    plt.tight_layout()
    # plt.show()
    plt.savefig('../figure/opt' + list(phivalue).index(phi).__str__() +'.pdf')


# prior belif -> ultimate exit time
lmax = -5
teemax = 8
teevalue = np.linspace(start=0, stop=teemax, num=20)
phimin = 0.01
phimax = 1.81
l0value = np.linspace(start=-lmax, stop=lmax, num = 30)


phivalue = np.linspace(start=phimin, stop=phimax, num = 7)
norm = clrs.Normalize(phimin, phimax)

plt.close('all')
for phi in phivalue:
    col = clrmap(norm(phi))
    taueult= []
    for l0 in l0value:
        if (l0 > lmin(phi, 0) and veprime(l0, 0, phi, 0) > 0):
            try:
                taueult.extend([newton(
                    lambda taue: (exp(-r * taue) * (1-phi/r) + quad(lambda s: exp(-r*s) * funcf(s), taue, np.inf)[0]
                                  ) * funcf(taue) / (exp(fint(taue)-l0)+1) - phi * exp(-r * taue),
                    1, maxiter= 99)])
                # print(str(l0) + str(phi))
            except:
                next
        else:
            break
        # try:
        #     taueult.extend([brentq(
        #    lambda taue: (exp(-r * taue) * (1-phi/r) + quad(lambda s: exp(-r*s) * funcf(s), taue, np.inf)[0]
        #     ) * funcf(taue) / (exp(fint(taue)-l0)+1) - phi * exp(-r * taue),
        #    0, 7)])
        # except:
        #     break

    plt.plot(l0value[0:taueult.__len__()], taueult, color=col, marker='o', label = str(np.round(phi,2)))

plt.xlabel("Prior belief $\\ell_0$")
plt.ylabel("Ultimate exit time $\\tau_E^{\\bar{*}}$")
sm = plt.cm.ScalarMappable(cmap=clrmap, norm=plt.Normalize(vmin = phimin, vmax= phimax))
plt.colorbar(sm, ticks = phivalue, label='Running cost $\\varphi$')
# plt.legend(bbox_to_anchor=(1, 1),loc = 2, borderaxespad=0.7, title = 'Running cost $\\varphi$')
axes = plt.gca()
axes.set_ylim([0, teemax])
plt.tight_layout()
plt.show()
# plt.savefig('../figure/ult.pdf')



colors = [[0, "limegreen"],
          [1, "navy"]]
clrmap = clrs.LinearSegmentedColormap.from_list("", colors)

plt.close('all')
ymin = -3
phimax = 2
phimin = 0.00001
phivalue = np.concatenate([np.linspace(start = phimin, stop = 0.24, num = 999),
                           np.linspace(start = 0.241, stop = 1.8, num = 99),
                           np.linspace(start = 1.81, stop = phimax, num = 999)], axis=0)
# plt.plot(phivalue, [lmin(i) for i in phivalue], color='0.2')
teemax = 20
teevalue = np.linspace(start=0, stop=teemax, num=5)
philims = []
ls = []

phi0 = (1 + quad(lambda s: exp(-r * s) * funcf(s), 0, np.inf)[0]
                     )/(1/r+1/funcf(0))
plt.axvline(x = phi0, color='red', linestyle = '--', label = '$\\varphi^*$')

norm = clrs.Normalize(0, teemax)
for tee in teevalue:
    col = clrmap(norm(tee))
    lmins = []
    philim = (1 + exp(r * tee) * quad(lambda s: exp(-r * s) * funcf(s), tee, np.inf)[0]
                     )/(1/r+1/funcf(tee))
    philims.extend([philim])
    for phi in phivalue:
        if phi < philim:
            l = lmin(phi, tee)
            lmins.extend([l])
        else:
            ls.extend([l])
            break

    plt.plot(phivalue[0:lmins.__len__()], lmins, color=col)

# plt.stem(philims, ls, linefmt  = 'r--', markerfmt=' ', basefmt=' ', bottom= ymin,
#          use_line_collection = True)
plt.axvspan(phi0, phimax, alpha=0.1, color = 'k')
plt.xlabel("Running cost $\\varphi$")
plt.ylabel("Minimum prior belief $\\ell_{0*}(t,\\varphi)$")
sm = plt.cm.ScalarMappable(cmap=clrmap, norm=plt.Normalize(vmin=0, vmax=teemax))
plt.colorbar(sm, ticks=teevalue, label='Trading age $t$')
plt.legend(loc = 2)
axes = plt.gca()
axes.set_xlim([0, phimax])
# axes.set_ylim([ymin, 45])
plt.tight_layout()
# plt.show()
plt.savefig('../figure/minl.pdf')

# nitr = 99999
# # probs = np.linspace(start=0, stop=1, num=nitr)[1:(nitr - 1)]
#
#
# def optexit(tee, l0, phi):
#     pit = funcpi(funcl(l0, tee))
#
#     # foo = np.random.exponential(1 / funcf(tee), nitr)
#     # foo = np.random.random(9999999)
#     foo = np.array([tauinv(i, tee) for i in probs]) # maybe better (more accurate and faster) than random sampling?!
#
#     def vexpt(taue): # gives the same value as vexpt2 and vexpt3 but much faster!
#         # def tempfun(x):
#         #     return quad(lambda s: exp(-r*s)*funcf(s), x, np.inf)[0]
#         # temp1 = quad(lambda x: (
#         #     exp(-r*x) + tempfun(x)
#         # ) * funcf(x) * exp(fint(tee) - fint(x)),
#         #              tee, taue)[0]
#         temp = (  # same as temp1 but much faster
#             exp(alp*(exp(bet*tee) - exp(bet*taue))/bet + tee * (gam+r)) * (
#             r*(alp*exp(bet*taue)+gam) - bet*gam
#         ) - exp(taue*(gam+r)) * (
#             r*(alp*exp(bet*tee)+gam) - bet*gam
#         )
#         ) / (exp(gam*taue + r*(tee+taue)) * r * (bet-r)
#              )
#         return -(pit * (
#                 temp + exp(fint(tee) - fint(taue)) * phi * exp(-r * taue) / r
#         ) + (1 - pit) * valuestochB(taue, phi))
#
#
#     # xx = np.linspace(start=0, stop=6.5, num=100)
#     # plt.plot(xx, [funcf(i) * exp(fint(tee) - fint(i)) for i in xx], color='red') # theoretical dist
#     # plt.plot(xx[2:], gaussian_kde(foo)(xx[2:])) # generated dist
#     # plt.show() #check it fits well with theoretical distribution (fits perfectly!)
#
#     # def vexpt2(taue):
#     #     temp = [valuestochG(taue, i, phi) for i in foo]
#     #     return -(pit * np.mean(temp) +
#     #              (1 - pit) * valuestochB(taue, phi))
#     #
#     # def vexpt3(taue): # gives the same value as vexpt2 but faster!
#     #     def valuestochG1(taon, phi):
#     #         return phi * exp(-r * taon) / r + (
#     #                 exp(-r * taon) * (1 + vg(taon, phi))
#     #         )
#     #     temp = [valuestochG1(i, phi) for i in foo[foo < taue]]
#     #     prob = exp(fint(tee) - fint(taue))
#     #     return -(pit * (
#     #             (1 - prob) * np.mean(temp) + prob * phi * exp(-r * taue) / r
#     #     ) + (1 - pit) * valuestochB(taue, phi))
#     #
#     # starttime = time()
#     # print('vexpt = ' + str(vexpt(4)))
#     # print(str(time() - starttime) + ' sec')
#     #
#     # starttime = time()
#     # print('vexpt2 = ' + str(vexpt2(4)))
#     # print(str(time() - starttime) + ' sec')
#     #
#     # starttime = time()
#     # print('vexpt3 = ' + str(vexpt3(4)))
#     # print(str(time() - starttime) + ' sec')
#
#     res = minimize(vexpt, tee + 0.5, tol=1e-3, method='Nelder-Mead'
#                    # , bounds=((tee,None),) # Nelder-Mead does not handle bounds
#                    )
#     return res.x
#
# phi = 0.01
# teevalue = np.linspace(start=0, stop=20, num=21)
#
#
# l0value = np.linspace(start=-2.5, stop=2.5, num=5)
#
# axes = plt.gca()
# axes.set_ylim([-0.1,15])
# axes.set_xlim([-0.1,15])
# plt.plot(range(8), color = 'red')
# plt.xlabel("Trading age $t$")
# plt.ylabel("Optimal exit time $\\tao_E$")
#
#
# for l0 in l0value[0:2]:
#     col = str(1-funcpi(funcl(l0, 0)))
#     taueopt = []
#     for tee in teevalue:
#         print(tee)
#         ans = optexit(l0, tee, phi)
#         taueopt.extend(ans)
#         print(ans)
#         if ans < tee:
#             break
#
#     plt.plot(teevalue[0:(taueopt.__len__())], taueopt, color = col)
#     plt.scatter(teevalue[0:(taueopt.__len__())], taueopt, color = col)
#
# plt.show()
#
#
# def finalexit(l0):
#     res = least_squares(lambda x: optexit(x, l0, phi) - x, x0=4, bounds=(0, np.inf))
#     return res.x
#
# upperbound = upbd(alp, bet, gam, r)
#
# nphi = 5
# phivalue = np.linspace(start=0.01, stop=upperbound, num=nphi)
#
# exits = [[] for i in phivalue]
#
# for i in range(nphi)[1:]:
#     phi = phivalue[i]
#     print(phi)
#     exits[i] = Parallel(n_jobs=10, backend="multiprocessing")(
#                     delayed(finalexit)(l0) for l0 in l0value
#                 )
