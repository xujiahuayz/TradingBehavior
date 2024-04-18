from functools import partial
from funcsJX import *
import pandas as pd
import multiprocessing
from time import time
import pickle


def murtdfunc(n, data):
    return murtd(tee=data[n:(n+1)]['tees'].item(),
                 m=data[n:(n+1)]['mugi'].item())

def maspfunc(n, data):
    return masp(tee=data[n:(n+1)]['tees'].item())

def murfunc(n, re, data):
    tmp = data[n:(n + 1)]
    return mur(re, tmp['tees'].item(), tmp['mugi'].item())


def muretafunc(n, re, data, **kwargs):
    gg = kwargs.get('gg', norm.pdf(re, mugg, sigg))
    bg = kwargs.get('bg', norm.pdf(re, mubg, sibg))
    gb = kwargs.get('gb', norm.pdf(re, mugb, sigb))
    bb = kwargs.get('bb', norm.pdf(re, mubb, sibb))
    tmp = data[n:(n + 1)]
    return mureta(re, tmp['tees'].item(), tmp['mugi'].item(),
                   gg=gg, bg=bg, gb=gb, bb=bb)


if __name__ == '__main__':
    dtatemp = pd.DataFrame()
    pool = multiprocessing.Pool()
    starttime = time()

    dtatemp = pd.DataFrame()
    dtatemp['tees'] = np.concatenate([[0.001, 0.005], np.linspace(0.01, 3.01, 76), np.linspace(3.2, 10.2, 36)])
    ntee = range(dtatemp['tees'].__len__())
    print('done tees')

    dtatemp['mugi'] = pool.map(muGinf, dtatemp['tees'])
    print('done mugi')

    dtatemp['masp'] = pool.map(partial(maspfunc, data=dtatemp), ntee)
    print('done masp')

    dtatemp['etap'] = pool.map(funcetap, dtatemp['tees'])
    print('done etap ' + str(time() - starttime))

    dtatemp['murtd'] = pool.map(partial(murtdfunc, data=dtatemp), ntee)
    print('done murtd' + str(time() - starttime))


    for ret in res:
        gg = norm.pdf(ret, mugg, sigg)
        bg = norm.pdf(ret, mubg, sibg)
        gb = norm.pdf(ret, mugb, sigb)
        bb = norm.pdf(ret, mubb, sibb)
        restr = ret.__round__(1).__str__()
        dtatemp['R' + restr] = pool.map(
            partial(murfunc, re=ret, data=dtatemp), ntee)
        dtatemp['mureta' + restr] = pool.map(partial(
            muretafunc, re=ret, data=dtatemp, gg=gg, bg=bg, gb=gb, bb=bb
        ), ntee)

        print(restr + ' in ' + str(time() - starttime))

    with open('murtdta6.pickle', 'wb') as f:
        pickle.dump(dtatemp, f)

    # dta = [[  # create empty list for data with different parameters
    # ] for i in ns]
    #
    # for i in ns:
    #
    #     dtatemp = pd.DataFrame()
    #     dtatemp['murt'] = pool.map(partial(murt, Time=Ts[i]), res)
    #     print('murt in ' + str(time() - starttime))
    #
    #     dtatemp['muretat'] = pool.map(partial(muretat, Time=Ts[i]), res)
    #     print('muretat in ' + str(time() - starttime))
    #
    #     dta[i] = dtatemp
    # with open('murt6.pickle', 'wb') as f:
    #     pickle.dump(dta, f)

