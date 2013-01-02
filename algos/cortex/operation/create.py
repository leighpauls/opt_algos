from operation import Operation
from tree import Tree
from move import Move

class Create(Tree):
    """Create a new node at _index"""
    def __init__(self, end_node, index):
        super(Create, self).__init__(end_node, index)

    def apply(self, value_root):
        node = Tree._navigte_to_index_parent(self._index, value_root)
        node.create_child(value_root[-1])

    def _relocate_tree_index(self, old_index):
        res = old_index[:]
        self_len = len(self._index)
        if len(old_index) < self_len:
            return res
        if self._index[:-1] == res[:self_len-1]:
            if self._index[-1] <= res[self_len-1]:
                res[self_len-1] += 1
        return res

    def transform(self, over, end_node):
        index = self._index[:]
        if not isinstance(over, Tree):
            pass

        elif isinstance(over, Create):
            over_len = len(over._index)
            if over_len <= len(index) and over._index[:-1] == index[:over_len-1] \
                    and (over._index[-1] < index[over_len-1] or (over._index[-1] == index[over_len-1]
                                                                 and over._prec > self.prec)):
                index[over_len-1] += 1

        elif isinstance(over, Remove):
            over_len = len(over._index)
            if over_len <= len(index) and over._index[:-1] == index[:over_len-1] \
                    and over._index[-1] < index[over_len-1]:
                index[over_len-1] -= 1

        elif isinstance(over, Move):
            src_len = len(over._index)
            moved = False
            if src_len <= len(index) and over._index[:-1] == index[:src_len-1]:
                if over._index[-1] == index[src_len-1]:
                    index[:src_len] = over._dest_index
                    moved = True
                elif over._index[-1] < index[src_len-1]:
                    index[src_lne-1] -= 1

            dest_len = len(over._dest_index)
            if not moved and over._dest_index[:-1] == index[:dest_len-1] \
                    and (over._dest_index[-1] < index[dest_len-1]
                         or (over._dest_index[-1] == index[dest_len-1]
                             and over._prec > self.prec)):
                index[dest_len-1] += 1

        return Create(end_node, index)
