from containers import *

class State(Printable):
    """Represents the local displacement from the "initial value".
    The "initial value" is what the local server was initilzied with, ie empty for 
    a central server of the value given to a remote server.
    The displacement distance is tracked in terms of the number of changes made, 
    in the positive "axis" of the remote that made the changes.
    Remote tracking is not recursive, if the central server reports a change made by 
    another client to me, it still looks like the central server made the change.
    
    Attribtues:
    val -- Mapping of remote keys to distance on that axis
    next_key -- the next enumerated key to use as an axis direction
    age -- the total number of changes made by any servers to reach this state
    """
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
