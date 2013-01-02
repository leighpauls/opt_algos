from operation import Operation 

class NoOp(Operation):
    def __init__(self, end_node, prec):
        Operation.__init__(self, end_node, prec)

    def apply(self, value_tree): pass
    def transform(self, over, end_node):
        return NoOp(end_node)
