from typing import Any, Iterable
from numpy.typing import NDArray
import numpy as np
from scipy.integrate import quad, nquad
from scipy.optimize import fsolve
from scipy.stats import norm
from scipy.misc import derivative
from learning.constants import (
    alp,
    bet,
    gam,
    r,
    sig,
    rouexit,
    nubarg,
    nubarb,
    lambdalg,
    lambdalb,
    upperbound,
)
from numba import njit

quadoptions = {"epsrel": 0.005}


# choose a function f(tee), monotone increasing ----
@njit
def funcf(tee: float) -> float:
    return alp * np.exp(bet * tee) + gam


## F(x), checked by mathematica
@njit
def fint(tee: float) -> float:
    return alp / bet * (np.exp(bet * tee) - 1) + gam * tee


# pi(l) ----
@njit
def funcpi(l: float) -> float:
    return 1 / (np.exp(-l) + 1)


# V_G(t) ----
@njit
def vg(tee: float, phi: float) -> float:
    return alp * np.exp(bet * tee) / (r - bet) + (gam - phi) / r


# l_*(t) function expanded thru mathematica (faster), no need to use Gamma(t) any more ----
@njit
def lstar(tee: float, phi: float) -> float:
    return np.log(
        (bet - r)
        * r
        * phi
        / (
            -(alp**2) * np.exp(2 * bet * tee) * r
            + (bet - r) * (gam + r) * (gam - phi)
            + alp
            * np.exp(bet * tee)
            * (bet * (gam + r - phi) + r * (-2 * gam - r + phi))
        )
    )


# f'(t)
@njit
def fdir(tee: float) -> float:
    return np.exp(bet * tee) * alp * bet


@njit
def lstardir(phi: float, tee: float) -> float:
    vgvar = vg(tee, phi)
    fvar = funcf(tee)
    return (fdir(tee) * (1 + vgvar) + fvar * (phi - fvar + r * vgvar)) / (
        phi - fvar * (1 + vgvar)
    )


## define ltilde_*(t, phi) ----
@njit
def ltilde(tee: float, phi: float) -> float:
    return lstar(tee, phi) + fint(tee)


# numerically calculate ltilde_*^(-1)(t, phi)
def ltildeinv(y: float, phi: float) -> float:
    return fsolve(lambda x: ltilde(x, phi) - y, 0.1).item()


# K(l,t) ----
def funck(l: float, tee: float, phi: float) -> float:
    return ltildeinv(l + fint(tee), phi)


# S(l,t) ----
def funcs(l: float, tee: float, phi: float) -> float:
    return tee - funck(l, tee, phi)


@njit
def tempz0(tau: float, k: float, lstarv: float, fintv: float) -> float:
    return r + funcpi(lstarv + fintv - fint(k + tau)) * funcf(k + tau)


def tempz(
    theta: float, k: float, s: float, phi: float, lstarv: float, fintv: float
) -> float:
    return (
        funcpi(lstarv + fintv - fint(k + theta))
        * funcf(k + theta)
        * (1 + vg(k + theta, phi))
        - phi
    ) * np.exp(quad(tempz0, theta, s, epsrel=0.05, args=(k, lstarv, fintv))[0])


# z(k,s) ---
def z(k: float, s: float, phi: float) -> float:
    lstarv = lstar(k, phi)
    fintv = fint(k)
    return quad(tempz, s, 0, epsrel=0.05, args=(k, s, phi, lstarv, fintv))[0]


# V(l,t)
def funcv(l: float, tee: float, phi: float) -> float:
    return z(funck(l, tee, phi), funcs(l, tee, phi), phi)


# V_l(l,t)
def vl(l: float, tee: float, phi: float) -> float:
    return derivative(lambda x: funcv(x, tee, phi), l, dx=1e-6)


# nubar(s)
@njit
def nubar(skills: bool) -> float:
    return np.where(skills, nubarg, nubarb).item()


# lambda_l(s)
@njit
def lambdal(skills: bool) -> float:
    return np.where(skills, lambdalg, lambdalb).item()


# entering agents draw their type from a distribution nu(l, phi, t, s)
@njit
def nus(skills: bool) -> float:
    return lambdal(skills) * nubar(skills) / upperbound


nut = nus(True)
nuf = nus(False)


# at the beginning we should have a lot of traders very confident
# so I changed the exponential distribution into inverse exponential distribution
@njit
def nul(l: float, phi: float, skills: bool) -> float:  
    x = lstar(tee=0, phi=phi) - l
    return np.exp(lambdal(skills) / x) / (x * x)


# mu(inf,t,G)
def tempmuG(tao: float, tee: float) -> float:
    temp = fint(tao)
    return (
        np.exp(-rouexit * tee - temp)
        * funcf(tao)
        * nquad(
            lambda x, y: nut * nul(x + temp, y, True),
            [lambda x, *xx: [lstar(tee=tao, phi=x), np.inf], [0, upperbound]],
            opts=[{"epsrel": 0.001}, {"epsrel": 0.001}],
        )[0]
    )


def muGinf(tee: float) -> float:
    return quad(tempmuG, 0, tee, epsrel=0.01, args=(tee,))[0]


# Total mass
@njit
def tempmus(l: float, phi: float, tee: float) -> float:
    temp = fint(tee)
    return np.exp(-rouexit * tee - temp) * nut * nul(l + temp, phi, True) + np.exp(
        -rouexit * tee
    ) * nuf * nul(l + temp, phi, False)


@njit
def tempmusOne(l: float, phi: float, tee: float, s: bool) -> float:
    temp = fint(tee)
    if s == True:
        nu = nut
    else:
        nu = nuf
    return np.exp(-rouexit * tee - temp * s) * nu * nul(l + temp, phi, s)


def maspOne(tee: float, s: bool) -> float:
    return nquad(
        lambda l, phi: tempmusOne(l, phi, tee, s),
        [lambda x, *xx: [lstar(tee=tee, phi=x), np.inf], [0, upperbound]],
        opts=[{"epsrel": 0.001}, {"epsrel": 0.001}, {"epsrel": 0.001}],
    )[0]


def masp(tee: float) -> float:
    return nquad(
        tempmus,
        [lambda x, *xx: [lstar(tee=tee, phi=x), np.inf], [0, upperbound]],
        opts=[{"epsrel": 0.001}, {"epsrel": 0.001}, {"epsrel": 0.001}],
        args=(tee,),
    )[0]


# to calculate total mass, use
# masp(tee) + muGinf(tee)


def ttlmasOne(Time: float, s: bool) -> float:
    if s == True:
        r = quad(lambda x: maspOne(x, True), 0, Time, epsrel=0.001)[0]
    else:
        r = quad(lambda x: maspOne(x, False), 0, Time, epsrel=0.001)[0]
    return r


def ttlmas(Time: float) -> float:
    return quad(lambda x: masp(x) + muGinf(x), 0, Time, epsrel=0.001)[0]


# cross-sectional distribution of returns
@njit
def tempmu(l: float, phi: float, tee: float, wt: NDArray, wf: NDArray) -> NDArray:
    return wt * tempmusOne(l, phi, tee, True) + wf * tempmusOne(l, phi, tee, False)


def aggreturnp(tee: float, wt: NDArray, wf: NDArray) -> float:
    return nquad(
        tempmu,
        [lambda x, *xx: [lstar(tee=tee, phi=x), np.inf], [0, upperbound]],
        opts=[{"epsrel": 0.01}, {"epsrel": 0.1}],
        args=(tee, wt, wf),
    )[0]


def mur(R: float, tee: float, *args) -> float:
    if args.__len__() > 0:
        m = args[0]
    else:
        m = muGinf(tee=tee)
    wt = norm.pdf(R, funcf(tee), sig)
    wf = norm.pdf(R, 0, 1)
    return aggreturnp(tee, wt, wf) + m * wt # type: ignore


def murt(R: float, Time: float) -> float:
    return quad(lambda x: mur(R=R, tee=x), 0, Time, epsrel=0.01)[0]


def murtd(tee: float, **kwargs) -> float:
    m = kwargs.get("m", muGinf(tee=tee))  # pre-calculate infinity
    return quad(lambda x: x * mur(x, tee, m), -np.inf, np.inf, epsrel=0.001)[0]


# Total exit
@njit
def tempexp(phi: float, tee: float) -> float:
    lstarv = lstar(tee, phi)
    return tempmus(lstarv, phi, tee)


@njit
def endoexit(phi: float, tee: float) -> float:
    return tempexp(phi, tee) * (1 + lstardir(phi, tee))


def exitratep(phi: float, tee: float, s: bool) -> float:
    lstarv = lstar(tee, phi)
    return (1 + lstardir(phi, tee)) * tempmusOne(lstarv, phi, tee, s)


def exitrate(tee: float, s: bool) -> float:
    return quad(lambda phi: exitratep(phi, tee, s), 0, upperbound, epsrel=0.001)[0]


#
# def exitrateBp(phi, tee):
#     lstarv = lstar(tee, phi)
#     return (1 + lstardir(phi, tee)) * tempmusOne(lstarv, phi, tee, True)


@njit
def tempexpOne(phi: float, tee: float, s: bool) -> float:
    lstarv = lstar(tee, phi)
    return tempmusOne(lstarv, phi, tee, s)


def exitp(tee: float) -> float:
    return quad(tempexp, 0, upperbound, args=(tee,), epsrel=0.001)[0]


def exitpOne(tee: float, s: bool) -> float:
    return quad(tempexpOne, 0, upperbound, args=(tee, s), epsrel=0.001)[0]


# to calculate exit, use
# (1-rouexit) * exitp(tee) + rouexit * (total mass)


# Distribution of returns of those who exit
def murexitp(R: float, tee: float) -> float:
    wt = norm.pdf(R, funcf(tee), sig)
    wf = norm.pdf(R, 0, 1)
    return quad(
        lambda x: tempmu(l=lstar(tee, x), phi=x, tee=tee, wt=wt, wf=wf),
        0,
        upperbound,
        epsrel=0.01,
    )[0]


# \int \mu^R_{exit}(R, t) dt
def murtexitc(R: float, Time: float) -> float:
    return quad(lambda x: murexitp(R=R, tee=x), 0, Time, epsrel=0.01)[0]


# to calculate murexit use
# inconf = murexitp(R, tee, **kwargs)
# rouexit * mur(T, T, t)  + (1-rouexit)* inconf


# ## F^{-1}(x), checked by mathematica
# @njit
# def fintinv(x):
#     temp = (alp + bet * x - gam * lambertw(
#         alp * exp((alp + bet * x) / gam) / gam
#     )) / (bet * gam)
#     return temp.real
#
#
# ## tau^{-1}(x), checked by mathematica
# @njit
# def tauinv(p, tee):
#     return fintinv(fint(tee) - log(1 - p))
