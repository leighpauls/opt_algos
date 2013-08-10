from operation import Operation

class Insert(Operation):
    def __init__(self, end_node, prec, pos, val):
        Operation.__init__(self, end_node, prec)
        self._pos = pos
        self._val = val
        
    def apply(self, value_list):
        value_list.insert(self._pos, self._val)


    def transform(self, over, end_node):
        from delete import Delete
        pos = self._pos
        if isinstance(over, Insert):
            if over._pos < pos or (over._pos == pos and over._prec > self._prec):
                pos += 1
        elif isinstance(over, Delete):
            if over._pos < pos:
                pos -= 1

        return Insert(end_node, self._prec, pos, self._val)

    OP_NAME = 'INSERT'

    @property
    def pos(self):
        return self._pos
    @property
    def val(self):
        return self._val
