from client_node import ClientNode
import copy
from printable import Printable

class Operation(Printable):
    NO_OP = "NO_OP"
    INSERT = "INSERT"
    DELETE = "DELETE"

    """An edge in the history graph
    Attribtues:
    op -- The operation this will do, (NO_OP, INSERT, DELETE)
    pos -- The enumerated positon to apply the operation at
    val -- the value to use if it's an INSERT operation
    end -- The ClientNode type at the end of this edge
    prec -- The precedence of the client that made the operation
    """
    def __init__(self, operation, position, value, end_node, precedence):
        self.op = operation
        self.pos = position
        self.val = value
        self.end = end_node
        self.prec = precedence

def from_server_change(change, source_node):
    """Creates a new operation with a bank node accoring to a sever's Change""" 
    return Operation(operation=change.op,
                     position=change.pos,
                     value=change.val,
                     end_node=ClientNode(server_state=source_node.server_state + 1,
                                   local_state=source_node.local_state),
                     precedence=change.prec)


def transform(transform_op, over_op, end_node):
    """Compute the transform of an operation
    Params:
    transform_op -- the Operation to find the equivilent of
    over_op -- the Operation to compute the transoform over
    end_node -- the ClientNode the result should be pointing to
    Returns:
    A new Operation pointing at end_node
    """
    res = copy.copy(transform_op)
    res.end = end_node

    from operation import Operation
    if over_op.op == Operation.INSERT:
        if over_op.pos < res.pos \
                or (over_op.pos == res.pos and over_op.prec > res.prec):
            res.pos += 1
    elif over_op.op == Operation.DELETE:
        raise "notimpl"
    elif over_op.op == Operation.NO_OP:
        None
    else:
        raise "Unknown operation: " + over_op.op

    return res
