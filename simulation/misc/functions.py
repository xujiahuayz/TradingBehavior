import pandas as pd
from numpy import exp
from numpy import log
from scipy.integrate import quad
from scipy.integrate import nquad
from scipy.optimize import fsolve
from scipy.stats import norm
from scipy.special import lambertw
import itertools
from ParaScope import *


def expand_grid(dct):
    rows = itertools.product(*dct.values())
    return pd.DataFrame.from_records(rows, columns=dct.keys())


# pi(l) ----
def funcpi(l):
    return 1 / (exp(-l) + 1)


# choose a function f(tee), monotone increasing ----
def funcf(tee):
    return alp * exp(bet * tee) + gam


## F(x), checked by mathematica
def fint(x):
    return alp / bet * (exp(bet * x) - 1) + gam * x


## F^{-1}(x), checked by mathematica
def fintinv(x):
    temp = (alp + bet * x - gam * lambertw(
        alp * exp((alp + bet * x) / gam) / gam
    )) / (bet * gam)
    return temp.real


## tau^{-1}(x), checked by mathematica
def tauinv(p, tee):
    return fintinv(fint(tee) - log(1 - p))


# V_G(t) ----
def vg(tee, phi):
    # def temp(s):
    #     return exp(-r * (s - tee) + bet * s) * alp + exp(-r * (s - tee)) * gam
    # return quad(temp, tee, np.inf)[0] - phi/r
    return alp * exp(bet * tee) / (r - bet) + (gam - phi) / r


# l_*(t) function expanded thru mathematica (faster), no need to use Gamma(t) any more ----
def lstar(tee, phi):
    return log(
        (bet - r) * r * phi / (
                -alp ** 2 * exp(2 * bet * tee) * r + (bet - r) * (gam + r) * (gam - phi) + alp * exp(bet * tee) * (
                bet * (gam + r - phi) + r * (-2 * gam - r + phi)
        )
        )
    )


'''
# define Gamma(t) ----
def funcgamma(tee, phi):
    return phi / (funcf(tee) * (1 + vg(tee, phi)))

def lstarold(tee, phi):
    return log(funcgamma(tee, phi) / (1 - funcgamma(tee, phi)))

lstar is 3-5 times faster than lstarold

starttime = time()
lstarold(2,0.05)
print(time()-starttime)

starttime = time()
lstar(2,0.05)
print(time()-starttime)
'''


## define ltilde_*(t, phi), function expanded thru mathematica ----
def ltilde(tee, phi):
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
def ltildeinv(y, phi):
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
def funck(l, tee, phi):
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
def funcs(l, tee, phi):
    return tee - funck(l, tee, phi)


# starttime = time()
# print(funcs(2, 1, 0.3))
# print(time()-starttime) #0.0005 seconds

## eta(l,t, phi)
def funceta(l, tee, phi):
    return -vl(l, tee, phi) * funcf(tee) + funcpi(l) * funcf(tee) * (
            1 + vg(tee, phi) - funcv(l, tee, phi))


# V(l,t)
def funcv(l, tee, phi):
    return z(funck(l, tee, phi), funcs(l, tee, phi), phi)


# V_l(l,t)
def vl(l, tee, phi):
    epsilon = 0.0001
    return (funcv(l + epsilon, tee, phi) - funcv(l, tee, phi)) / epsilon


# z(k,s) ---
def z(k, s, phi):
    def temp(theta):
        def temp0(tau):
            return r + funcpi(lstar(k, phi) + fint(k) - fint(k + tau)) * funcf(k + tau)

        return (funcpi(
            lstar(k, phi) + fint(k) - fint(k + theta)
        ) * funcf(k + theta) * (1 + vg(k + theta, phi)) - phi
                ) * exp(quad(
            temp0, theta, s, epsrel=0.05
        )[0])

    return quad(
        temp, 0, s, epsrel=0.05
    )[0] * (-1)


# starttime = time()
# print('z = ' + str(z(9,14,0.9)))
# print('V_l = ' + str(vl(9,14,0.9)))
# print(str(time()-starttime) + ' sec')

# need to check again
def funcmu(Time, l, tee, phi, skills):
    def temp(x):
        return rouentry * exp(
            -rouexit * (Time - x) + (
                (- fint(tee) + fint(tee - (Time - x))) if skills else 0
            )
        ) * nu(l + fint(tee) - fint(tee - (Time - x)), 1 / phi, tee - (Time - x), skills)

    return quad(temp, max(Time - tee, 0), Time)[0]
    # else:
    #     # def temp(x):
    #     #     return rouentry * exp(-rouexit * (Time - x)
    #     #                           ) * nu(l + fint(tee) - fint(tee - (Time - x)), 1 / phi, tee - (Time - x), skills)
    # return (1-exp(-rouexit * (max(Time - tee, 0)-Time))) * rouentry / rouexit


# mu(T,inf,t,G)
def muGinf(Time, tee):
    def temp(tao):
        def temp1(phi):
            def temp0(l):
                return funcmu(tao, l, tee - Time + tao, phi, True)

            return quad(temp0, lstar(tee - Time + tao, phi), np.inf, epsrel=0.1)[0]

        return exp(-rouexit * (Time - tao)) * funcf(tee - Time + tao) * quad(
            temp1, 0, upperbound,
            epsrel=0.1)[0]

    return quad(temp, max(Time - tee, 0), Time, epsrel=0.5)[0]


# cross-sectional distribution of returns
def mur(R, Time, tee):
    def temp1(phi):
        def temp0(l):
            return (funcpi(l) * norm.pdf(R, mugg, sigg) + (1 - funcpi(l)) * norm.pdf(R, mubg, sibg)
                    ) * funcmu(Time, l, tee, phi, True) + (
                           funcpi(l) * norm.pdf(R, mugb, sigb) + (1 - funcpi(l)) * norm.pdf(R, mubb, sibb)
                   ) * funcmu(Time, l, tee, phi, False)

        return quad(temp0, lstar(tee, phi), np.inf, epsrel=0.05)[0]

    temp2 = quad(temp1, 0, upperbound, epsrel=0.05)[0]
    return temp2 + muGinf(Time, tee) * norm.pdf(R, mugg, sigg)


# Total mass
def mas(Time, tee):
    def temp(l):
        def temp0(phi):
            return funcmu(Time, l, tee, phi, False) + funcmu(Time, l, tee, phi, True)

        return quad(temp0, 0, upperbound)[0]

    temp1 = quad(temp, 0, np.inf)[0]
    return temp1 + muGinf(Time, tee)


# Total exit
def exit(Time, tee):
    def temp(phi):
        return funcmu(Time, lstar(tee, phi), tee, phi, True) + funcmu(Time, lstar(tee, phi), tee, phi, False)

    return quad(temp, 0, upperbound)[0] + rouexit * mas(Time, tee)


# Distribution of returns of those who exit
def murexit(R, Time, tee):
    def temp(phi):
        funcpi(lstar(tee, phi)) * norm.pdf(R, mugg, sigg) + (
                1 - funcpi(lstar(tee, phi))
        ) * norm.pdf(R, mubg, sibg) * funcmu(Time, lstar(tee, phi), tee, phi, True) + (
                funcpi(lstar(tee, phi)) * norm.pdf(R, mugb, sigb) + (
                1 - funcpi(lstar(tee, phi))
        ) * norm.pdf(R, mubb, sibb) * funcmu(Time, lstar(tee, phi), tee, phi, False)
        )

    return quad(temp, 0, upperbound)[0] + rouexit * mur(R, Time, tee)


# nubar(s)
def nubar(skills):
    return np.where(skills, nubarg, nubarb).item()


# lambda_l(s)
def lambdal(skills):
    return np.where(skills, lambdalg, lambdalb).item()


# entering agents draw their type from a distribution nu(l, phi, t, s)
def nu(l, phi, tee, skills):
    return nubar(skills) * lambdal(skills) * exp(
        -lambdal(skills) * l) * 1 / upperbound * (phi > 0) * (
                   phi < upperbound) * lbda * exp(-lbda * tee)


# integrand for entry rate
def entryint(l, phi, tee):
    return nu(l, phi, tee, True) + nu(l, phi, tee, False)


def paibad(l0, tee, t0, phi):
    if l0 - fint(tee + t0) + fint(t0) < lstar(tee + t0, phi):
        return 0
    else:
        return exp(-rouentry * tee)


# probability
def prblt(Time, tee, t0):
    def paigood(l0):
        if l0 - fint(tee + t0) + fint(t0) < lstar(tee + t0, phi):
            return 0
        else:
            return exp(-rouentry * tee)

    quad(paibad(t0, l0, True), -np.inf, np.inf, epsrel=0.05)[0]


# E_t[tao_exit > T], probability of survival
# until time T at time t for good type who has age a and type l
def exittime(Time, tee, a, l0, phi):
    def temp(s):
        return funcf(s) * exp(fint(tee) - fint(s)) * paibad(
            l0, s - tee, a, phi) * exp(-rouexit * (Time - s))

    return exp(fint(tee) - fint(Time)) * paibad(l0, Time - tee, a, phi) + quad(
        temp, tee, Time
    )[0]


def upbd(alp, bet, gam, r):
    return (alp + gam - 0.5 * (r + bet)
            ) if alp > 0 else (
            alp + gam - 0.5 * (r + alp * bet / (alp + gam)))


# calculate eta and entry rate, later using parallel computing
def ent(r_v):
    global r
    r = r_v

    global upperbound
    upperbound = upbd(alp, bet, gam, r)

    phi_value = np.linspace(start=0.0001, stop=upperbound, num=8)
    dflstar = expand_grid(
        {'tee': tee_value, 'phi': phi_value}
    )

    dflstar['lst'] = dflstar.apply(lambda x: lstar(x['tee'], x['phi']), axis=1)

    df = expand_grid({'l': l_value,
                      'tee': tee_value,
                      'phi': phi_value}
                     )

    df = df.merge(dflstar)
    df = df[df['l'] > df['lst']]

    df['eta'] = df.swifter.apply(lambda x: funceta(x['l'], x['tee'], x['phi']), axis=1)

    entry = nquad(entryint, [
        lambda phi, tee: [lstar(tee, phi), np.inf],
        [0, upperbound],
        [0, np.inf]
    ], opts=[quadoptions, quadoptions, quadoptions])[0]

    return dict(plotdata=df, entrydata=entry)


def funcl(l0, tee):
    return l0 - fint(tee)


def funcl(l0, tee):
    return l0 - fint(tee)


def pdetaun(tau, l0, tee):
    return funcpi(funcl(l0, tau)) * funcf(tau) * exp(fint(tee) - fint(tau))


def ve(l0, tee, phi, taue):
    def temp(taun):
        return (exp(-r * taun) + quad(lambda s: exp(-r * s) * funcf(s), taun, np.inf)[0]
                ) * pdetaun(taun, l0, tee)

    return -phi * exp(-r * tee) / r + quad(temp, tee, taue)[0] + phi * exp(-r * taue) / r * (
            1 - quad(lambda taun: pdetaun(taun, l0, tee), tee, taue)[0])


def veprime(l0, tee, phi, taue):
    return (exp(-r * taue) * (1 - phi / r) + quad(lambda s: exp(-r * s) * funcf(s), taue, np.inf)[0]
            ) * pdetaun(taue, l0, tee) - phi * exp(-r * taue) * (
                   1 - quad(lambda taun: pdetaun(taun, l0, tee), tee, taue)[0])

# minimum value of l0
def lmin(phi, tee):
    return fint(tee) -log(
        funcf(tee)*(1 - phi / r + exp(r * tee) * quad(lambda s: exp(-r * s) * funcf(s), tee, np.inf)[0]
         ) / phi - 1
    )


def pmf(tee, ns):
    return sum([(fint(tee)**n)/(exp(fint(tee)) * np.math.factorial(n)) for n in range(ns)])


def tmeans(n):
    def dens(tee, n):
        return (fint(tee) ** (n - 1) * funcf(tee)) / (exp(fint(tee)) * np.math.factorial(n - 1))
    return quad(lambda tee: tee*dens(tee, n), 0, np.inf)[0]

# # second half of the value function for type G
# def valuestochG(taoe, taon, phi):
# return phi * exp(-r * min(taoe, taon))/r + (
#     0 if taoe <= taon else exp(-r * taon) * (1 + vg(taon, phi))
# )
# # second half of the value function for type B
#
# def valuestochB(taoe, phi):
# return phi * exp(-r * taoe)/r
