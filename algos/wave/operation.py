from node import Node
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
    end -- The Node type at the end of this edge
    prec -- The precedence of the client that made the operation
    """
    def __init__(self, operation, position, value, end_node, precedence):
        self.op = operation
        self.pos = position
        self.val = value
        self.end = end_node
        self.prec = precedence

    def transform(transform_op, over_op, end_node):
        """Compute the transform of an operation
        Params:
        transform_op -- the Operation to find the equivilent of
        over_op -- the Operation to compute the transoform over
        end_node -- the Node the result should be pointing to
        Returns:
        A new Operation pointing at end_node
        """
        res = copy.copy(transform_op)
        res.end = end_node

        from Operation import NO_OP, INSERT, DELETE        
        if over_op.op == INSERT:
            if over_op.pos < res.pos \
                    or (over_op.pos == res.pos and over_op.prec > res.prec):
                res.pos += 1
        elif over_op.op == DELETE:
            raise "notimpl"
        elif over_op.op == NO_OP:
            None
        else:
            raise "Unknown operation: " + over_op.op

        return res
