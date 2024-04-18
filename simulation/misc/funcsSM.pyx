import pandas as pd
import numpy as np
from numpy import exp, log
from scipy.integrate import quad, nquad
from scipy.optimize import fsolve
from scipy.stats import norm
from scipy.special import lambertw
from scipy.misc import derivative
import itertools

cdef float nubarg = 0.3
cdef float nubarb = 0.05

cdef float lambdalg = 0.7
cdef float lambdalb = 0.8

cdef float lbda = 3
cdef float mugg = 0.3
cdef float mugb = 0.2
cdef float mubg = 0.2
cdef float mubb = 0.1

cdef float sigg = 1
cdef float sigb = 1
cdef float sibg = 1
cdef float sibb = 1

cdef float rouentry = 0.6
cdef float rouexit = 0.4

cdef float alp = -0.2
cdef float bet = -0.02
cdef float gam = 2
cdef float r = 0.1

cdef float upperbound = (alp + gam - 0.5 * (r + bet)
              ) if alp > 0 else (
        alp + gam - 0.5 * (r + alp * bet / (alp + gam)))

quadoptions = {'epsrel': 0.005}

def expand_grid(dct):
    rows = itertools.product(*dct.values())
    return pd.DataFrame.from_records(rows, columns=dct.keys())

# choose a function f(tee), monotone increasing ----
cpdef float funcf(float tee):
    return alp * exp(bet * tee) + gam

## F(x), checked by mathematica
cpdef float fint(float tee):
    return alp / bet * (exp(bet * tee) - 1) + gam * tee


# pi(l) ----
cpdef float funcpi(float l):
    return 1 / (exp(-l) + 1)


## F^{-1}(x), checked by mathematica
cpdef float fintinv(float x):
    temp = (alp + bet * x - gam * lambertw(
        alp * exp((alp + bet * x) / gam) / gam
    )) / (bet * gam)
    return temp.real


## tau^{-1}(x), checked by mathematica
cpdef float tauinv(float p, float tee):
    return fintinv(fint(tee) - log(1 - p))


# V_G(t) ----
cpdef float vg(float tee, float phi):
    return alp * exp(bet * tee) / (r - bet) + (gam - phi) / r


# l_*(t) function expanded thru mathematica (faster), no need to use Gamma(t) any more ----
cpdef float lstar(float tee, float phi):
    return log(
        (bet - r) * r * phi / (
                -alp ** 2 * exp(2 * bet * tee) * r + (bet - r) * (gam + r) * (gam - phi) + alp * exp(bet * tee) * (
                bet * (gam + r - phi) + r * (-2 * gam - r + phi)
        )
        )
    )


## define ltilde_*(t, phi), function expanded thru mathematica ----
cpdef float ltilde(float tee, float phi):
    return alp / bet * (exp(bet * tee) - 1) + gam * tee + log(
        (bet - r) * r * phi / (
                -alp ** 2 * exp(2 * bet * tee) * r + (bet - r) * (gam + r) * (gam - phi) + alp * exp(bet * tee) * (
                bet * (gam + r - phi) + r * (-2 * gam - r + phi)
        )
        )
    )


'''
def ltildeold(tee, phi):
    return lstar(tee, phi) + fint(tee)

ltilde is 2-3 times faster than ltildeold

starttime = time()
ltildeold(2,0.05)
print(time()-starttime)

starttime = time()
ltilde(2,0.05)
print(time()-starttime)
'''


# numerically calculate ltilde_*^(-1)(t, phi)
cdef float ltildeinv(float y, float phi):
    return fsolve(lambda x: ltilde(x, phi) - y, 0.1).item()


'''
# def ltildeprime(tee, phi):
#    return gammaprime(tee, phi) / (funcgamma(tee, phi) * (1 - funcgamma(tee, phi))) + funcf(tee)

def ltildeprime(x, phi):
    return (alp**3*exp(3*bet*x)*r - gam*(
            bet - r)*(gam + r)*(gam - phi) + alp * exp(
        bet*x)*(gam*r*(3*gam + 2*r) - bet*(
            2*gam**2 + 4*gam*r + r**2) + bet**2*(
            gam + r - phi) + 2*bet*(gam + r) * phi - r*(
            2*gam + r)*phi) - alp**2*exp(2*bet*x)*(
            bet*(gam + 3*r - phi) + r*(-3*gam - r + phi)))/(
            alp**2*exp(2*bet*x)*r - (bet - r)*(gam + r)*(
            gam - phi) - alp * exp(bet * x) * (
            bet*(gam + r - phi) + r*(-2*gam - r + phi)))

## ltilde_*^{-1}'(y) ----
def ltildeinvprime(y, phi):
   return 1 / ltildeprime(ltildeinv(y, phi), phi)
'''


# K(l,t) ----
cpdef float funck(float l, float tee, float phi):
    return ltildeinv(l + fint(tee), phi)


# starttime = time()
# print(funck(2, 1, 0.3))
# print(time()-starttime) #0.0005 seconds

'''
def funck(l, tee, phi):
   return ltildeinv(alp / bet * (exp(bet * tee) - 1) + gam * tee + l, phi)

funcknew is not necessarily faster!

starttime = time()
funck(1,2,0.05)
print(time()-starttime)

starttime = time()
funcknew(1,2,0.05)
print(time()-starttime)
'''


# S(l,t) ----
cpdef float funcs(float l, float tee, float phi):
    return tee - funck(l, tee, phi)


# starttime = time()
# print(funcs(2, 1, 0.3))
# print(time()-starttime) #0.0005 seconds

## eta(l,t, phi)
cpdef float funceta(float l, float tee, float phi):
    return -vl(l, tee, phi) * funcf(tee) + funcpi(l) * funcf(tee) * (
            1 + vg(tee, phi) - funcv(l, tee, phi))


# V(l,t)
cdef float funcv(float l, float tee, float phi):
    return z(funck(l, tee, phi), funcs(l, tee, phi), phi)


# V_l(l,t)
cdef float vl(float l, float tee, float phi):
    return derivative(
        lambda x: funcv(x, tee, phi), l, dx=1e-6
    )

    # epsilon = 0.0001
    # return (funcv(l + epsilon, tee, phi) - funcv(l, tee, phi)) / epsilon


cpdef float tempz0(float tau, float theta, float k, float s, float phi):
    return r + funcpi(lstar(k, phi) + fint(k) - fint(k + tau)) * funcf(k + tau)

cpdef float tempz(float theta, float k, float s, float phi):
    return (funcpi(
            lstar(k, phi) + fint(k) - fint(k + theta)
        ) * funcf(k + theta) * (1 + vg(k + theta, phi)) - phi
                ) * exp(quad(
            tempz0, theta, s, epsrel=0.05, args=(theta, k, s, phi)
        )[0])
# z(k,s) ---
cpdef float z(float k, float s, float phi):
    return quad(
        tempz, s, 0, epsrel=0.05, args=(k, s, phi)
    )[0]


# need to check again
cpdef float tempmu(float x, float Time, float l, float tee, float phi, bint skills):
    return exp(
            -rouexit * (Time - x) + (
                (- fint(tee) + fint(tee - (Time - x))) if skills else 0
            )
        ) * nu(l + fint(tee) - fint(tee - (Time - x)), phi, tee - (Time - x), skills)

cpdef float funcmu(float Time, float l, float tee, float phi, bint skills):
    return rouentry * quad(tempmu, max(Time - tee, 0), Time,
                           args = (Time, l, tee, phi, skills), epsrel=0.05
                           )[0]

# mu(T,inf,t,G)
cpdef float tempmuG0(float l, float phi, float tao, float Time, float tee):
    return funcmu(tao, l, tee - Time + tao, phi, True)

cpdef float tempmuG1(float phi, float tao, float Time, float tee):
    return quad(tempmuG0, lstar(tee - Time + tao, phi), np.inf, epsrel=0.1,
                args=(phi, tao, Time, tee))[0]

cpdef float tempmuG(float tao, float Time, float tee):
    return exp(-rouexit * (Time - tao)) * funcf(tee - Time + tao) * quad(
        tempmuG1, 0, upperbound,
        epsrel=0.2, args=(tao, Time, tee))[0]

cpdef float muGinf(float Time, float tee):
    return quad(tempmuG, max(Time - tee, 0), Time, epsrel=0.1,
                args=(Time, tee))[0]

# cross-sectional distribution of returns
cpdef float tempmur0(float l, float phi, float R, float Time, float tee):
    return (funcpi(l) * norm.pdf(R, mugg, sigg) + (1 - funcpi(l)) * norm.pdf(R, mubg, sibg)
            ) * funcmu(Time, l, tee, phi, True) + (
            funcpi(l) * norm.pdf(R, mugb, sigb) + (1 - funcpi(l)) * norm.pdf(R, mubb, sibb)
    ) * funcmu(Time, l, tee, phi, False)

cpdef float tempmur1(float phi, float R, float Time, float tee):
    return quad(tempmur0, lstar(tee, phi), np.inf, epsrel=0.1,
                args=(phi, R, Time, tee))[0]

cpdef float mur(float R, float Time, float tee):
    temp2 = quad(tempmur1, 0, upperbound, epsrel=0.1, args=(R, Time, tee))[0]
    return temp2 + muGinf(Time, tee) * norm.pdf(R, mugg, sigg)


# Total mass
cpdef float tempmas0(float l, float phi, float Time, float tee):
    return funcmu(Time, l, tee, phi, False) + funcmu(Time, l, tee, phi, True)

cpdef float tempmas(float phi, float Time, float tee):
    return quad(tempmas0, lstar(tee, phi), np.inf, args=(phi, Time, tee))[0]

cpdef float mas(float Time, float tee):
    temp1 = quad(tempmas, 0, upperbound, args=(Time, tee))[0]
    return temp1 + muGinf(Time, tee)


cpdef float tempmurtd(x, tee, Time):
    return x*mur(x, Time, tee)/mas(Time, tee)

cpdef float murtdmurtd(float tee, float Time = 10):
    return quad(tempmurtd, -np.inf, np.inf, epsrel=0.1, args=(tee, Time))[0]


# marginal distribution of \muR on t
cpdef float tempmmurt(x, float tee, float Time):
    return x*mur(x, Time, tee)

cpdef float mmurt(float tee, float Time = 10):
    return quad(tempmmurt, -np.inf, np.inf, epsrel=0.1,
                args=(tee, Time))[0]


# Total exit
cpdef float tempexit(float phi, float Time, float tee):
    return funcmu(Time, lstar(tee, phi), tee, phi, True) + funcmu(Time, lstar(tee, phi), tee, phi, False)

cpdef float exit(float Time, float tee):
    return quad(tempexit, 0, upperbound,
                args=(Time, tee))[0] + rouexit * mas(Time, tee)

# Distribution of returns of those who exit
cpdef float tempmurexit(float phi, float R, float Time, float tee):
    return funcpi(lstar(tee, phi)) * norm.pdf(R, mugg, sigg) + (
                1 - funcpi(lstar(tee, phi))
        ) * norm.pdf(R, mubg, sibg) * funcmu(Time, lstar(tee, phi), tee, phi, True) + (
                funcpi(lstar(tee, phi)) * norm.pdf(R, mugb, sigb) + (
                1 - funcpi(lstar(tee, phi))
        ) * norm.pdf(R, mubb, sibb) * funcmu(Time, lstar(tee, phi), tee, phi, False)
        )

cpdef float murexit(float R, float Time, float tee):
    return quad(tempmurexit, 0, upperbound,
                args=(R, Time, tee))[0] + rouexit * mur(R, Time, tee)


# nubar(s)
cpdef float nubar(bint skills):
    return np.where(skills, nubarg, nubarb).item()


# lambda_l(s)
cpdef float lambdal(bint skills):
    return np.where(skills, lambdalg, lambdalb).item()


# entering agents draw their type from a distribution nu(l, phi, t, s)
cpdef float nu(float l, float phi, float tee, bint skills):
    return nubar(skills) * lambdal(skills) * exp(
        -lambdal(skills) * l) * 1 / upperbound * (phi > 0) * (
                   phi < upperbound) * lbda * exp(-lbda * tee)

# integrand for entry rate
cpdef float entryint(float l, float phi, float tee):
    return nu(l, phi, tee, True) + nu(l, phi, tee, False)


cpdef float paibad(float l0, float tee, float t0, float phi):
    if l0 - fint(tee + t0) + fint(t0) < lstar(tee + t0, phi):
        return 0
    else:
        return exp(-rouentry * tee)


cpdef float upbd(float alp, float bet, float gam, float r):
    return (alp + gam - 0.5 * (r + bet)
            ) if alp > 0 else (
            alp + gam - 0.5 * (r + alp * bet / (alp + gam)))

cpdef float funcl(float l0, float tee):
    return l0 - fint(tee)
