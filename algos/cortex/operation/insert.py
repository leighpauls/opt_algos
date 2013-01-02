from value import Value


class Insert(Value):
    """Insert Value Operation
    Attributes:
    value -- the value to insert at _tree_index,_linear_index
    """
    def __init__(self, end_node, prec, tree_index, linear_index, value):
        Value.__init__(self, end_node, prec, tree_index, linear_index)
        self._value = value

    def apply(self, value_root):
        node = self._navigate_to_tree_node(value_root)
        node.insert_value(self._linear_index, self._value)

    def transform(self, over, end_node):
        from delete import Delete
        # find the tree node to apply to
        tree_index = self._get_xformed_tree_index(over)
        if tree_index is None:
            return Operation.NoOp(end_node)

        # find the location in the value to apply to
        linear_index = self._linear_index
        if isinstance(over, Insert):
            if over._linear_index < linear_index or (
                over._linear_index == lienar_index and over._prec > self.prec):
                linear_index += 1
        elif isinstance(over, Delete):
            if over._linear_index < linear_index:
                linear_index -= 1

        return Insert(end_node, self._prec, tree_index, linear_index, self._value)
