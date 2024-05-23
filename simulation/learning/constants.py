from learning.settings import PROJECT_ROOT
import numpy as np

FIG_DIR = PROJECT_ROOT / "figures"
DATA_DIR = PROJECT_ROOT / "data"

alp = -0.5
bet = -0.2
gam = 1
r = 0.1

# standard deviation of return for type G
sig = 0.9

rouexit = 0.005

# when traders join they are mostly very confident, then confidence decreases
# lambda of \ell distribution for good traders
lambdalg = 6  # the larger, the larger mean

# lambda of \ell distribution for good traders
lambdalb = 5  # the larger, the larger mean

# no need for lbda if there is no distribution for age
# lbda = 3

nubarg = 0.5
nubarb = 1 - nubarg


# check: upperbound must exceed 0!!!
upperbound = (
    (alp + gam - 0.5 * (r + bet))
    if alp > 0
    else ((alp + gam) - (r + alp * bet / (alp + gam)) / 2)
)

# ((alp + gam) * r * (1 + gam / r + alp / (-bet + r))) / (alp + gam + r)


res = np.linspace(-4, 4, 41)

Ts = np.array([1, 5, 10])

ns = range(len(Ts))

teev = np.concatenate(
    [
        np.array([0.001, 0.01, 0.03]),
        np.linspace(0.05, 0.95, 19),
        np.linspace(1, 3.5, 26),
        np.linspace(4, 20, 17),
    ]
)
