from cortex_node import CortexNode

class Initializer:
    """An object to be passed from the Server to create a new Client
    Attributes:
    remote_id -- the ID assigned to the remote
    precedence -- the precedence of the remote
    initial_value -- CortexNode to start the client with
    initial_state -- the server state number at the given value
    """
    def __init__(self, remote_id, precedence, initial_value, initial_state):
        self.remote_id = remote_id
        self.precedence = precedence
        self.initial_value = initial_value
        self.initial_state = initial_state

    def to_dict(self):
        return {"remote_id": self.remote_id,
                "precedence": self.precedence,
                "initial_value": self.initial_value.to_dict(),
                "initial_state": self.initial_state}

    @staticmethod
    def from_dict(obj):
        return Initializer(
            obj["remote_id"],
            obj["precedence"],
            CortexNode.from_dict(obj["initial_value"]),
            obj["initial_state"])
