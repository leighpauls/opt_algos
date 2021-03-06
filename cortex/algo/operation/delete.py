from value import Value

class Delete(Value):
    """Delete Value Operation, deletes value at _tree_index,_linear_index"""
    
    OP_NAME="DELETE"

    def __init__(self, end_node, prec, tree_index, linear_index):
        Value.__init__(self, end_node, prec, tree_index, linear_index)

    class Event(Value.Event):
        def __init__(self, target_node):
            self.OP_NAME = Delete.OP_NAME
            Value.Event.__init__(self, target_node)
            
    def apply(self, value_root):
        node = self._navigate_to_tree_node(value_root)
        node.delete_value(self._linear_index)
        node.trigger_event(self.Event(node))

    def transform(self, over, end_node):
        from insert import Insert
        # find the tree node to apply to
        tree_index = self._get_xformed_tree_index(over)
        if tree_index is None:
            from no_op import NoOp
            return NoOp(end_node, self._prec)

        # find where in the value to apply the delete
        linear_index = self._linear_index
        if isinstance(over, Value) and over._tree_index == tree_index:
            if isinstance(over, Insert):
                if over._linear_index <= linear_index:
                    linear_index += 1
            elif isinstance(over, Delete):
                if over._linear_index < linear_index:
                    linear_index -= 1
                elif over._linear_index == linear_index:
                    from no_op import NoOp
                    return NoOp(end_node, self._prec)

        return Delete(end_node, self._prec, tree_index, linear_index)
