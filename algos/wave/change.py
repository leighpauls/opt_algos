from printable import Printable

class Change(Printable):
    """Represents a change, readable by both Server and Client
    Attributes:
    src_clinet_state -- The source state as understood by the client sending/recieveing this message
    src_server_state -- The source state of the server, relative to the client state sending/recieving this message
    op -- The operation identifier
    pos -- position to apply the operation
    val -- value for an INSERT operation
    """
    def __init__(self, src_client_state, src_rel_server_state, op, pos, val):
        self.src_client_state = src_client_state
        self.src_rel_server_state = src_rel_server_state
        self.op = op
        self.pos = pos
        self.val = val
        
