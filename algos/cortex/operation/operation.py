class Operation(object):
    def __init__(self, end_node, prec):
        self._end_node = end_node
        self._prec = prec

    @staticmethod
    def from_change(change, end_node):
        """Creates a new operation according to a change"""
        from no_op import NoOp
        from insert import Insert
        from delete import Delete
        from create import Create
        from move import Move

        if change.op == Insert.OP_NAME:
            return Insert(end_node=end_node,
                          prec=change.prec,
                          tree_index=change.tree_index,
                          linear_index=change.linear_index,
                          val=change.val)
        elif change.op == Delete.OP_NAME:
            return Delete(end_node=end_node,
                          prec=change.prec,
                          tree_index=change.tree_index,
                          linear_index=change.linear_index)
        elif change.op == NoOp.OP_NAME:
            return NoOp(end_node=end_node,
                        prec=prec)
        elif change.op == Create.OP_NAME:
            return Create(end_node=end_node,
                          prec=change.prec,
                          index=change.tree_index)
        elif change.op == Remove.OP_NAME:
            return Remove(end_node=end_node,
                          prec=change.prec,
                          index=change.tree_index)
        elif change.op == Move.OP_NAME:
            return Move(end_node=end_node,
                        prec=change.prec,
                        index=change.tree_index,
                        dest_index=change.dest_tree_index)
        else:
            raise "Unknown OP_NAME: \"" + change.op + "\""

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


    # properties for override
    @property
    def pos(self):
        return None
    @property
    def val(self):
        return None
    @property
    def tree_index(self):
        return None
    @property
    def dest_tree_index(self):
        return None
    @property
    def linear_index(self):
        return None

    

