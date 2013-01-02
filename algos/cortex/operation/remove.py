from tree import Tree

class Remove(Tree):
    """Delete the node at _index"""
    def __init__(self, end_node, index):
        Tree.__init__(self, end_node, index)

    def apply(self, value_root):
        node = Tree._navigte_to_index_parent(self._index, value_root)
        node.pop_child(value_root[-1])

    def _relocate_tree_index(self, old_index):
        res = old_index[:]
        self_len = len(self._index)
        if len(old_index) < self_len:
            return res
        if self._index[:-1] == res[:self_len-1]:
            if self._index[-1] == res[self_len-1]:
                return None
            elif self._index[-1] < res[self_len-1]:
                res[self_len-1] -= 1
        return res

    def transform(self, over, end_node):
        index = self._index[:]
        if not isinstance(over, Tree):
            pass

        elif isinstance(over, Tree.Insert):
            over_len = len(over._index)
            if over_len <= len(index) and over._index[:-1] == index[:over_len-1] \
                    and over._index[-1] <= index[over_len-1]:
                index[over_len-1] += 1

        elif isinstance(over, Remove):
            over_len = len(over._index)
            if over_len <= len(index) and over._index[:-1] == index[:over_len-1]:
                if over._index[-1] == index[over_len-1]:
                    return Operation.NoOp(end_node, self.prec)
                elif over._index[-1] < index[over_len-1]:
                    index[over_len-1] -= 1

        elif isinstance(over, Tree.Move):
            src_len = len(over._index)
            moved = False
            if src_len <= len(index) and over._index[:-1] == index[:src_len-1]:
                if over._index[-1] == index[src_len-1]:
                    index[:src_len] = over._dest_index
                    moved = True
                elif over._index[-1] < index[src_len-1]:
                    index[src_len-1] -= 1

            dest_len = len(over._dest_index)
            if not moved and dest_len < len(index) and over._dest_index[:-1] == index[:dest_len-1] \
                    and over._dest_index[-1] <= index[dest_len-1]:
                index[dest_len-1] += 1

        return Delete(end_node, index)
