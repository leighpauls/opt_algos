from operation import Operation

class NoOp(Operation):
    def apply(self, value_list):
        pass

    def transform(self, over, end_node):
        return NoOp(end_node, self._prec)

    OP_NAME = 'NO_OP'
