from .. import Printable
import copy

class State(Printable):
    """A point in state-space of the server's history
    Attributes:
    _val -- the state vector at this node
    _birth_state -- the total age of this node
    """
    def __init__(self, copy_state=None):
        if copy_state is not None:
            self._val = copy.copy(copy_state._val)
            self._birth_state = copy_state._birth_state
        else:
            self._val = {}
            self._birth_state = 0

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
        self._birth_state += 1

    def get_relative_state(self, reference_axis):
        """Get the total state relative to the reference axis"""
        return self._birth_state - self.get_axis_state(reference_axis)
            
    def get_axis_state(self, axis):
        """Get the value on the given axis"""
        if axis in self._val.keys():
            return self._val[axis]
        return 0

    def age_to_include(self, other):
        """If needed, make this node older so that all of it's axies are <= other's
        assumes that <self> contains all axies that <other> knows about"""
        for axis, state in self._val.iteritems():
            diff = state - other.get_axis_state(axis)
            if diff > 0:
                self._val[axis] -= diff
                self._birth_state -= diff
                
    def is_younger_than(self, other):
        """Is <self> younger in any axies than <other>
        Could be reciprocally true"""
        for axis, state in self._val.iteritems():
            if axis not in other._val:
                # I know about something he doesn't, I must be younger
                return True
            if state > other._val[axis]:
                return True
        return False
