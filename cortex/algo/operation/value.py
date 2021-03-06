from operation import Operation

class Value(Operation):
    """Operations modifying the value of a node
    Attributes:
    _tree_index -- index list to the node being modified
    _linear_index -- location in that node's value being modified
    """
    def __init__(self, end_node, prec, tree_index, linear_index):
        Operation.__init__(self, end_node, prec)
        self._tree_index = tree_index
        self._linear_index = linear_index

    class Event(object):
        def __init__(self, target_node):
            self._target_node = target_node
        @property
        def target_node(self):
            return self._target_node
        @property
        def linear_index(self):
            return self._linear_index

    def _navigate_to_tree_node(self, value_root):
        cur_node = value_root
        for index in self._tree_index:
            cur_node = cur_node.get_child(index)
        return cur_node

    def _get_xformed_tree_index(self, over):
        from tree import Tree
        if not isinstance(over, Tree):
            return self._tree_index[:]
        return over._relocate_tree_index(self._tree_index)

    @property
    def tree_index(self):
        return self._tree_index
    @property
    def linear_index(self):
        return self._linear_index
