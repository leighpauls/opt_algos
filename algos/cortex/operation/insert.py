from value import Value
from no_op import NoOp


class Insert(Value):
    """Insert Value Operation
    Attributes:
    value -- the value to insert at _tree_index,_linear_index
    """
    OP_NAME = "INSERT"

    def __init__(self, end_node, prec, tree_index, linear_index, val):
        Value.__init__(self, end_node, prec, tree_index, linear_index)
        self._value = val

    class Event(Value.Event):
        def __init__(self, target_node):
            self.OP_NAME = Insert.OP_NAME
            Value.Event.__init__(self, target_node)
        @property
        def value(self):
            return self._value

    def apply(self, value_root):
        node = self._navigate_to_tree_node(value_root)
        node.insert_value(self._linear_index, self._value)
        node.trigger_event(self.Event(node))

    def transform(self, over, end_node):
        from delete import Delete
        # find the tree node to apply to
        tree_index = self._get_xformed_tree_index(over)
        if tree_index is None:
            return NoOp(end_node, self._prec)

        # find the location in the value to apply to
        linear_index = self._linear_index
        if isinstance(over, Value) and over._tree_index == tree_index:
            if isinstance(over, Insert):
                if (over._linear_index < linear_index
                    or (over._linear_index == linear_index
                        and over._prec > self._prec)):
                    linear_index += 1
            elif isinstance(over, Delete):
                if over._linear_index < linear_index:
                    linear_index -= 1

        return Insert(
            end_node,
            self._prec,
            tree_index,
            linear_index,
            self._value)

    @property
    def val(self):
        return self._value
