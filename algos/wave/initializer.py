
class Initializer:
    """An object to be passed from the Server to create a new Client
    Attributes:
    remote_id -- the ID assigned to the remote
    precedence -- the precedence of the remote
    initial_value -- Enumerable value to start the client with
    initial_state -- the server state number at the given value
    """
    def __init__(self, remote_id, precedence, initial_value, initial_state):
        if type(initial_state) is not int:
            raise "Invalid initial state"
        self.remote_id = remote_id
        self.precedence = precedence
        self.initial_value = initial_value
        self._initial_state = initial_state
        
    @property
    def initial_state(self):
        return self._initial_state
