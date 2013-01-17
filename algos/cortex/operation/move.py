from tree import Tree
from no_op import NoOp

class Move(Tree):
    """Move the node at _index to _dest_index
    Removes the node at _index, then re-inserts it at _dest_index after the later sister nodes have been shifted to fill the gap
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
        temp = Tree._navigate_to_index_parent(self._index, value_root).pop_child(self._index[-1])
        dest_parent = Tree._navigate_to_index_parent(self._dest_index, value_root)
        dest_parent.insert_child(self._dest_index[-1], temp)

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
                    return NoOp(end_node, self._prec)
                elif over._index[-1] < src_index[over_len-1]:
                    src_index[over_len-1] -= 1

            if over_len <= len(dest_index) and over._index[:-1] == dest_index[:over_len-1]:
                if over._index[-1] == dest_index[over_len-1]:
                    return Remove(end_node, self._prec, src_index)
                elif over._index[-1] < dest_index[over_len-1]:
                    dest_index[over_len-1] -= 1


        elif isinstance(over, Move):
            over_src_len = len(over._index)
            over_dest_len = len(over._dest_index)
            my_src_len = len(src_index)
            my_dest_len = len(dest_index)
            
            # move my src idx
            src_moved = False
            if over_src_len <= my_src_len and over._index[:-1] == src_index[:over_src_len-1]:
                if over._index[-1] == src_index[over_src_len-1]:
                    src_moved = True
                    src_index[:over_src_len] = over._dest_index
                elif over._index[-1] < src_index[over_src_len-1]:
                    src_index[over_src_len-1] -= 1
            if not src_moved and over_dest_len <= my_src_len and over._dest_index[:-1] == src_index[:over_dest_len-1] \
                    and over._dest_index[-1] <= src_index[over_dest_len-1]:
                src_index[over_dest_len-1] += 1

            dest_moved = False
            if over_src_len <= my_dest_len and over._index[:-1] == dest_index[:over_src_len-1]:
                if over._index[-1] == dest_index[over_src_len-1] and over_src_len < my_dest_len:
                    dest_moved = True
                    dest_index[:over_src_len] = over._dest_index
                elif over._index[-1] < dest_index[over_src_len-1]:
                    dest_index[over_src_len-1] -= 1
            if not dest_moved and over_dest_len <= my_dest_len and over._dest_index[:-1] == dest_index[over_dest_len-1] \
                    and (over._dest_index[-1] < src_index[over_dest_len-1] or (over._dest_index[-1] == src_index[over_dest_len-1] and over._prec > self._prec)):
                dest_index[over_dest_len-1] += 1

        return Move(end_node, self._prec, src_index, dest_index)

    @property
    def dest_tree_index(self):
        return self._dest_index

    def to_csv_cell(self):
        return "MOV " + str(self._index) + " to " + str(self._dest_index) + " p" + str(self._prec)
