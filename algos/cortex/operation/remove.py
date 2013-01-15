from tree import Tree

class Remove(Tree):
    """Delete the node at _index"""

    OP_NAME="REMOVE"
    
    def __init__(self, end_node, prec, index):
        Tree.__init__(self, end_node, prec, index)

    def apply(self, value_root):
        node = Tree._navigate_to_index_parent(self._index, value_root)
        node.pop_child(self._index[-1])

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
        from create import Create
        from move import Move
        
        index = self._index[:]
        if not isinstance(over, Tree):
            pass

        elif isinstance(over, Create):
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

        elif isinstance(over, Move):
            src_len = len(over._index)
            moved = False
            if src_len <= len(index) and over._index[:-1] == index[:src_len-1]:
                if over._index[-1] == index[src_len-1]:
                    index[:src_len] = over._dest_index
                    moved = True
                elif over._index[-1] < index[src_len-1]:
                    index[src_len-1] -= 1

            dest_len = len(over._dest_index)
            if not moved and dest_len <= len(index) and over._dest_index[:-1] == index[:dest_len-1] \
                    and over._dest_index[-1] <= index[dest_len-1]:
                index[dest_len-1] += 1

        return Remove(end_node, self._prec, index)

    def to_csv_cell(self):
        return "REM " + str(self._index) + " p" + str(self._prec)
