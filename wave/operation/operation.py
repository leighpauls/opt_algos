import copy

class Operation(object):
    """An edge in the history graph
    Attribtues:
    op -- The operation this will do, (NO_OP, INSERT, DELETE)
    pos -- The enumerated positon to apply the operation at
    val -- the value to use if it's an INSERT operation
    end -- The ClientNode/ServerNode type at the end of this edge
    prec -- The precedence of the client that made the operation
    """
    def __init__(self, end_node, prec):
        self._end_node = end_node
        self._prec = prec

    @staticmethod
    def from_change(change, end_node):
        """Creates a new operation according to a change"""
        from delete import Delete
        from insert import Insert
        from no_op import NoOp

        if change.op == Insert.OP_NAME:
            return Insert(end_node=end_node,
                          prec=change.prec,
                          pos=change.pos,
                          val=change.val)
        elif change.op == Delete.OP_NAME:
            return Delete(end_node=end_node,
                          prec=change.prec,
                          pos=change.pos)
        elif change.op == NoOp.OP_NAME:
            return NoOp(end_node=end_node,
                        prec=change.prec)

    @property
    def op(self):
        return self.__class__.OP_NAME
    @property
    def prec(self):
        return self._prec
    @property
    def end(self):
        return self._end_node
    @end.setter
    def end(self, val):
        self._end_node = val
    
    # to be overriden
    @property
    def pos(self):
        return None
    @property
    def val(self):
        return None
    
