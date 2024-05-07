from __future__ import annotations


class TradingEnv:
    def __init__(self):
        self.time = 0
        self.users: dict[int, User] = {}

    def increment_time(self, time_increase: float):
        self.time += time_increase


class User:
    def __init__(self, id_no: int, env: TradingEnv):
        self.age = 0
        self.id = id_no
        self.env = env
        self.env.users[self.id] = self


class GoodUser(User):
    def __init__(self, id_no: int, env: TradingEnv):
        super().__init__(id_no, env)
        self.is_good = True


class BadUser(User):
    def __init__(self, id_no: int, env: TradingEnv):
        super().__init__(id_no, env)
        self.is_good = False
