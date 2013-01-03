from tree import Tree

class Move(Tree):
    """Move the node at _index to _dest_index
    Attributes:
    _dest_index -- destination to insert the node moved from _index
    """

    OP_NAME="MOVE"
    
    def __init__(self, end_node, prec, index, dest_index):
        Tree.__init__(self, end_node, prec, index)
        self._dest_index = dest_index
        if len(self._index) < len(self._dest_index) and self._index == self._dest_index[:len(self._index)]:
            raise Exception("Tried to move node under it's self")

    def apply(self, value_root):
        fixed_dest = self._dest_index[:]
        index_len = len(self._index)
        if index_len <= len(fixed_dest) and self._index[-1] < fixed_dest[index_len-1]:
            fixed_dest[index_len-1] -= 1

        temp = Tree._navigate_to_index_parent(self._index, value_root).pop_child(self._index[-1])
        Tree._navigate_to_index_parent(fixed_dest, value_root).insert_child(
            fixed_dest[-1], temp)

    def _relocate_tree_index(self, old_index):
        res = old_index[:]
        src_idx_len = len(self._index)
        if len(res) >= src_idx_len and self._index[:-1] == res[:src_idx_len-1]:
            # if it was moved directly
            if self._index[-1] == res[src_idx_len-1]:
                res[:src_idx_len] = self._dest_index
                return res
            elif self._index[-1] < res[src_idx_len-1]:
                # treat it as a delete
                res[src_idx_len-1] -= 1

        dest_idx_len = len(self._dest_index)
        if len(res) >= dest_idx_len and self._dest_index[:-1] == res[:dest_idx_len-1]:
            # treat it as an insert
            if self._dest_index[-1] <= res[dest_idx_len-1]:
                res[dest_idx_len-1] += 1
        return res

    def transform(self, over, end_node):
        from create import Create
        from remove import Remove
        from move import Move

        src_index = self._index[:]
        dest_index = self._dest_index[:]
        if not isinstance(over, Tree):
            pass

        elif isinstance(over, Create):
            over_len = len(over._index)
            if over_len <= len(src_index) and over._index[:-1] == src_index[:over_len-1] \
                    and over._index[-1] <= src_index[over_len-1]:
                src_index[over_len-1] += 1

            if over_len <= len(dest_index) and over._index[:-1] == dest_index[:over_len-1] \
                    and (over._index[-1] < dest_index[over_len-1]
                         or (over._index[-1] == dest_index[over_len-1] and over._prec > self.prec)):
                dest_index[over_len-1] += 1

        elif isinstance(over, Remove):
            over_len = len(over._index)
            if over_len <= len(src_index) and over._index[:-1] == src_index[:over_len-1]:
                if over._index[-1] == src_index[over_len-1]:
                    return Operation.NoOp(end_node, self._prec)
                elif over._index[-1] < src_index[over_len-1]:
                    src_index[over_len-1] -= 1

            if over_len <= len(dest_index) and over._index[:-1] == dest_index[:over_len-1] \
                    and over._index[-1] < dest_index[over_len-1]:
                dest_index[over_len-1] -= 1

        elif isinstance(over, Tree.Move):
            over_src_len = len(over._index)
            over_dest_len = len(over._dest_index)
            src_moved = False
            # move my source index according to over's source index?
            if over_src_len <= len(src_index) and over._index[:-1] == src_index[:over_src_len-1]:
                if over._index[-1] == src_index[over_src_len-1]:
                    src_index[:over_src_len] = over._dest_index
                    src_moved = True
                elif over._index[-1] < src_index[over_src_len-1]:
                    src_index[over_src_len-1] -= 1

            # move my source index according to over's dest index?
            if not src_moved and over_dest_len <= len(src_index) \
                    and over._dest_index[:-1] == src_index[:over_dest_len-1] \
                    and over._dest_index[-1] <= src_index[over_dest_len-1]:
                src_index[over_dest_len-1] += 1

            dest_moved = False
            # move my dest index according to over's source index?
            if over_src_len <= len(dest_index) and over._index[:-1] == dest_index[:over_src_len-1]:
                if over._index[-1] == dest_index[over_src_len-1]:
                    dest_index[:over_src_len] = over._dest_index
                    dest_moved = True
                elif over._index[-1] < dest_index[over_src_len-1]:
                    dest_index[over_src_len-1] -= 1

            # move my dest index according to over's dest index?
            if not dest_moved and over_dest_len < len(dest_index) \
                    and over._dest_index[:-1] == dest_index[:over_dest_len-1] \
                    and (over._dest_index[-1] < dest_index[over_dest_len-1]
                         or (over._dest_index[-1] == dest_index[over_dest_len-1]
                             and over._prec > self.prec)):
                dest_index[over_dest_len-1] += 1
        return Move(end_node, src_index, dest_index)

    @property
    def dest_tree_index(self):
        return self._dest_index
