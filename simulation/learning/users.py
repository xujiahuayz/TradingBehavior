from __future__ import annotations

from matplotlib import pyplot as plt
import numpy as np
from learning.funcsfinal import NonHomoPoisDist, funcf, vg
from learning.constants import r
from scipy.integrate import quad


class TradingEnv:
    def __init__(self, discount_rate: float = r):

        self.time = 0
        self.discount_rate = discount_rate
        self.users: dict[int, _User] = {}

    def increment_time(self, time_increase: float):
        self.time += time_increase


class _User:
    def __init__(self, id_no: int, prior_belief: float, cost: float, env: TradingEnv):
        self.age = 0  # tee
        assert 0 < prior_belief < 1, "prior belief of being G type must be within (0,1)"

        self.prior_belief = prior_belief
        self.cost = cost
        self.belief = self.prior_belief
        self.id = id_no
        self.env = env
        self.env.users[self.id] = self

    @property
    def ell(self) -> float:
        return np.log(self.belief / (1 - self.belief))


class GoodUser(_User):
    def __init__(self, id_no: int, prior_belief: float, cost: float, env: TradingEnv):
        super().__init__(id_no, prior_belief, cost, env)
        self.is_good = True
        non_homo_pois_dist = NonHomoPoisDist(current_age=0)
        self.first_jump: float = non_homo_pois_dist.rvs(1)

    def simulate_future_jumps(self, n: int) -> tuple[list[float], list[float]]:
        start_age = self.first_jump
        v = 1.0
        vs = [v]
        ages = [start_age]
        for i in range(n):
            non_homo_pois_dist = NonHomoPoisDist(current_age=start_age)
            jump_interval = non_homo_pois_dist.rvs(1)
            v *= np.exp(self.env.discount_rate * jump_interval)
            start_age += jump_interval
            # future value of wealth until right after this jump
            v += 1
            if i % 100 == 0:
                print(i)
            vs.append(v)
            ages.append(start_age)

        return ages, vs

        # first jump time of time-inhomogeneous Poisson process with rate f(t)
        # poisson_rate = funcf(self.age)


class BadUser(_User):
    def __init__(self, id_no: int, prior_belief: float, cost: float, env: TradingEnv):
        super().__init__(id_no, prior_belief, cost, env)
        self.is_good = False
        self.first_jump = np.inf


if __name__ == "__main__":
    trad_env = TradingEnv(discount_rate=r)
    alice = GoodUser(id_no=1, prior_belief=0.2, cost=0, env=trad_env)
    vg_values = []
    for _ in range(100):
        jump_age, returns = alice.simulate_future_jumps(int(500))
        discounted_returns = []
        for i, a in enumerate(jump_age):
            discounted_returns.append(returns[i] / np.exp(trad_env.discount_rate * a))
        vg_values.append(discounted_returns[-1])
    # the value converges
    plt.plot(jump_age[:100], discounted_returns[:100])
    np.mean(vg_values)
    vg(tee=0, phi=0)
    quad(lambda s: np.exp(-r * (s - 0)) * funcf(s), 0, np.inf)
