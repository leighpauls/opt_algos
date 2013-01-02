from operation import Operation

class Tree(Operation):
    """Operations modifying the tree structure of the document
    Attributes:
    _index -- list of child indicies to apply the operation at
    """
    def __init__(self, end_node, index):
        super(Tree, self).__init__(end_node)
        self._index = index

    @staticmethod
    def _navigate_to_index_parent(index, tree_root):
        if len(index) < 1:
            raise CorOptError("Tried to navigate to root's parent")
        cur_node = tree_root
        for i in index[:-1]:
            cur_node = cur_node.get_child(i)
        return cur_node
