from containers import *

class State(Printable):
    _INTERNAL_KEY = 0
    def __init__(self, age=0):
        self.val = { State._INTERNAL_KEY: 0 }
        self.next_key = State._INTERNAL_KEY + 1
        self.age = age
    def add_remote(self):
        res = self.next_key
        self.val[res] = 0
        self.next_key += 1
        return res
    def apply_local_change(self):
        self.val[State._INTERNAL_KEY] += 1
        self.age += 1
    def apply_remote_change(self, remote):
        self.val[remote] += 1
        self.age += 1
    def get_snapshot(self):
        return self.val.copy()
    def get_relative_to_remote(self, remote_key):
        remote_state = self.val[remote_key]
        return (self.age - remote_state, remote_state)
