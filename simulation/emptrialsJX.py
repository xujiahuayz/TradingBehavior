from learning.funcsfinal import *
from time import time

tee = 1
phi = 1


teev = np.linspace(0.1, 150.1, 51)

teev = 15


starttime = time()
yy1 = murtd(teev)
print(time() - starttime)
print(yy1)

starttime = time()
yy2 = aggreturn(teev)
print(time() - starttime)
print(yy2)

R = 0
bigT = 10
gg = norm.pdf(R, mugg, sigg)
bg = norm.pdf(R, mubg, sibg)
gb = norm.pdf(R, mugb, sigb)
bb = norm.pdf(R, mubb, sibb)

xx = quad(lambda x: mur(R, x), 0, bigT, epsrel=0.001)[0]
print(xx)

yy = murt(R, bigT)
print(yy)

xx = quad(lambda x: mur(x, teev), -np.inf, np.inf, epsrel=0.001)[0]
print(xx)
# 0.8979865977129745

yy = rouentry * masp(teev) + muGinf(teev)
print(yy)
# 0.8979853392633812

Tv = 10
starttime = time()
x0 = ttlmas(Time=Tv)
s = [str(i) for i in [Tv, x0, time() - starttime]]
print("ttlmas(Time = {}) = {} in {} secs".format(*s))
# ttlmas(Time = 0.1) = 0.04578412623454378 in 0.6520919799804688 secs
# ttlmas(Time = 1) = 0.2822780180588268 in 0.6433980464935303 secs
# ttlmas(Time = 10) = 0.8011563678419824 in 7.928161859512329 secs

# # immortal mass
# mug = muGinf(Time=Tv, tee=teev)
#
# # integrate \muR(R, T=Tv, t=teev) over R
# intmur = quad(lambda ret: mur(ret, Tv, teev, mug, t0), -np.inf, np.inf, epsrel=0.01)[0]
# print(intmur)
#
# # total mass \bar{M}(T=Tv, t=teev)
# masbar = rouentry * masp(Time=Tv, tee=teev, t0=t0) + mug
# print(masbar)
lv = 10
phiv = 0.5

starttime = time()
x1 = muGinf(tee=teev)
s = [str(i) for i in [teev, x1, time() - starttime]]
print("muGinf(tee = {}) = {} in {} secs".format(*s))
# muGinf(tee = 2) = 0.1430336271474283 in 0.019971370697021484 secs

starttime = time()
x4 = rouentry * masp(tee=teev) + x1
s = [str(i) for i in [teev, x4, time() - starttime]]
print("mas(tee = {}) = {} in {} secs".format(*s))
# mas(tee = 2) = 0.18257731676222413 in 0.04886817932128906 secs


starttime = time()
x7 = rouentry * exitp(tee=teev) + rouexit * x4
s = [str(i) for i in [teev, x7, time() - starttime]]
print("exit(tee = {}) = {} in {} secs".format(*s))
# exit(tee = 2) = 0.10145856873674008 in 0.0009720325469970703 secs

rv = 0.1

gg = norm.pdf(rv, mugg, sigg)
bg = norm.pdf(rv, mubg, sibg)
gb = norm.pdf(rv, mugb, sigb)
bb = norm.pdf(rv, mubb, sibb)

starttime = time()
x0 = muretat(R=rv)
s = [str(i) for i in [rv, x0, time() - starttime]]
print("muretat(R = {}) = {} in {} secs".format(*s))
# muretat(R = 0.1) = 0.1958079072594976 in 191.540132522583 secs


phiv = 1

starttime = time()
x13 = mureta(rv, teev, x1, gg=gg, bg=bg, gb=gb, bb=bb)
s = [str(i) for i in [rv, teev, x13, time() - starttime]]
print("mureta(R = {}, tee = {}) = {} in {} secs".format(*s))
# mureta(R = 0.1, tee = 2) = 0.0359219690882805 in 1.3424370288848877 secs


y1 = rouentry * tempmurex(phi=phiv, tee=teev, gg=gg, bg=bg, gb=gb, bb=bb)
print(y1)
# 6.253763458578977e-11

starttime = time()
x9 = rouentry * murexitp(R=rv, tee=teev)
s = [str(i) for i in [rv, teev, x9, time() - starttime]]
print("rouentry * murexitp(R = {}, tee = {}) = {} in {} secs".format(*s))
# rouentry * murexitp(R = 0.1, tee = 2) = 0.010924147792819179 in 0.001132965087890625 secs


starttime = time()
x6 = murt(R=rv)
s = [str(i) for i in [rv, x6, time() - starttime]]
print("murt(R = {}) = {} in {} secs".format(*s))
# murt(R = 0.1) = 0.39076227140834563 in 5.317758560180664 secs


starttime = time()
x3 = mur(rv, teev, x1)
s = [str(i) for i in [rv, teev, x3, time() - starttime]]
print("mur(R = {}, tee = {}) = {} in {} secs".format(*s))
# mur(R = 0.1, tee = 2) = 0.06913317986693421 in 0.0020170211791992188 secs

starttime = time()
x2 = murtd(tee=teev, m=x1)
s = [str(i) for i in [teev, x2, time() - starttime]]
print("murtd(tee = {}) = {} in {} secs".format(*s))
# murtd(tee = 2) = 0.04291008839552543 in 0.16634321212768555 secs


# starttime = time()
# x5 = murp(rv, Tv, teev, t0)
# # x3 = forig.mur(rv, Tv, teev, x1)
# s = [str(i) for i in [rv, Tv, teev, x5, time() - starttime]]
# print('murp(R = {}, Time = {}, tee = {}) = {} in {} secs'.format(*s))
# # murp(R = 0.3, Time = 10, tee = 2) = 0.08022144000428841 in 0.26728224754333496 secs
# # murp(R = 0.3, Time = 10, tee = 2) = 0.08022144000428841 in 0.2834756374359131 secs -- quad
# # murp(R = 0.3, Time = 10, tee = 2) = 0.05398980141827133 in 0.05487942695617676 secs -- n=10


starttime = time()
x11 = funceta(l=lv, tee=teev, phi=phiv)
s = [str(i) for i in [lv, teev, phiv, x11, time() - starttime]]
print("funceta(l={}, tee={}, phi={}) = {} in {} secs".format(*s))
# funceta(l=10, tee=2, phi=1) = 10.391671043106069 in 0.0009970664978027344 secs

starttime = time()
x1 = funcetap(tee=teev)
s = [str(i) for i in [teev, x1, time() - starttime]]
print("funcetap(tee = {}) = {} in {} secs".format(*s))
# funcetap(tee = 2) = 0.3433890330074522 in 3.3570427894592285 secs
