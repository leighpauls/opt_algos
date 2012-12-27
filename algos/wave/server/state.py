from .. import Printable
import copy

class State(Printable):
    """A point in state-space of the server's history
    Attributes:
    _val -- the state vector at this node
    _age -- the total age of this node
    """
    def __init__(self, copy_state=None):
        if copy_state is not None:
            self._val = copy.copy(copy_state._val)
            self._age = copy_state._age
        else:
            self._val = {}
            self._age = 0

    def append_state_axis(self, axis):
        """Add a new axis to this State Object"""
        if axis in self._val:
            raise "Tried to append over an existing state axis"
        self._val[axis] = 0
        
    def increment_state(self, axis):
        """Increment an axis in this State Object"""
        if axis not in self._val:
            raise "tried to increment a non-existant state axis"
        self._val[axis] += 1
        self._age += 1

    def get_relative_state(self, reference_axis):
        """Get the total state relative to the reference axis"""
        return self._age - self.get_axis_state(reference_axis)
            
    def get_axis_state(self, axis):
        """Get the value on the given axis"""
        if axis in self._val.keys():
            return self._val[axis]
        return 0
