from operation import Operation

class Delete(Operation):
    def __init__(self, end_node, prec, pos):
        Operation.__init__(self, end_node, prec)
        self._pos = pos

    def apply(self, value_list):
        value_list.pop(self._pos)

    def transform(self, over, end_node):
        from insert import Insert
        from no_op import NoOp
        pos = self._pos
        if isinstance(over, Insert):
            if over._pos <= pos:
                pos += 1
        elif isinstance(over, Delete):
            if over._pos < pos:
                pos -= 1
            elif over._pos == pos:
                return NoOp(end_node, self._prec)

        return Delete(end_node, self._prec, pos=pos)

    OP_NAME = 'DELETE'

    @property
    def pos(self):
        return self._pos
