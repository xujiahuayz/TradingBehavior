import importlib
import matplotlib.pyplot as plt
import funcs
importlib.reload(funcs)
from funcs import *
import pandas as pd
from scipy.optimize import curve_fit
from lifelines import KaplanMeierFitter
import pickle

df = pd.read_csv('regtable.csv', sep=',',
                 parse_dates=['first_tr', 'last_tr', 'closing_date', 'lastobs'])

kmf = KaplanMeierFitter()
kmf.fit(df['lifetime'], df['uncensored'])

trunc = 6330
t = kmf.timeline[0:trunc]/365.2425  # in years
q = np.array(kmf.survival_function_.iloc[0:trunc, 0])
plt.plot(t, q)
axes = plt.gca()
axes.set_ylim([0, 1])
plt.show()

# with open('surval.pickle', 'rb') as f:
#     surval = pickle.load(f)

# cinput = cvalue[0]
# rinput = rouexitvalue[1]
# linput = lbdvalue[0]
# ninput = nuvalue[1]
#
# def survf(d, nu, lbd, c, rouexit, alp, bet, gam):
#     return surv(d, nu=nu, lbd=lbd, c=c, rouexit=rouexit, alp=alp, bet=bet, gam=gam, r=1e-4)
#
# with open('surval2.pickle', 'rb') as f:
#     res = pickle.load(f)
#
# def survf(d, nu, lbd, c, rouexit, alp, bet, gam, r):
#     return surv(d, nu=nu, lbd=lbd, c=c, rouexit=rouexit, alp=alp, bet=bet, gam=gam, r=r)
starttime = time()

if __name__ == '__main__':
    def survfunc(deltat, nu=0.2, lbd=3, c=0, rouexit=0, alp=-0.2, bet=-0.02, gam=2, r=0.1):
        temp = partial(surv, nu=nu, lbd=lbd, c=c, rouexit=rouexit, alp=alp, bet=bet, gam=gam, r=r)
        pool = multiprocessing.Pool()
        return pool.map(temp, deltat)

    popt, pcov = curve_fit(survfunc, t, q,
                       p0=[0.25, 1, 0.6, 0.2, -0.2, -0.02, 2, 0.1],
                       bounds=([0, 0, 0, 0, -10, -1, 0, 0],
                               [3, 3, 5, 1, 0, 0, 5, 0.5]))
# popt, pcov = curve_fit(surv, deltats,
#                        res[paramlist.index((0.6, 1, 0.2, 0.25))],
#                        p0=[0.25, 1, 0.6, 0.2, -0.2, -0.02, 2, 0.1],
#                        bounds=([0, 0, 0, 0, -10, -1, 0, 0],
#                                [3, 3, 5, 1, 0, 0, 5, 0.5]))
    print(str(time() - starttime) + ' sec')
