from operation import Operation
from tree import Tree
from move import Move

class Create(Tree):
    """Create a new node at _index"""

    OP_NAME="CREATE"
    
    def __init__(self, end_node, prec, index):
        Tree.__init__(self, end_node, prec)
        self._index = index

    class Event(object):
        def __init__(self, new_node):
            self.OP_NAME = Create.OP_NAME
            self._new_node = new_node
        @property
        def new_node(self):
            return self._new_node

    def apply(self, value_root):
        parent_node = Tree._navigate_to_index_parent(self._index, value_root)
        new_node = parent_node.create_child(self._index[-1])
        parent_node.trigger_event(self.Event(new_node))

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
        from remove import Remove
        from move import Move
        from no_op import NoOp

        index = self._index[:]
        if not isinstance(over, Tree):
            pass

        elif isinstance(over, Create):
            over_len = len(over._index)
            if (over_len <= len(index)
                and over._index[:-1] == index[:over_len-1]
                and (over._index[-1] < index[over_len-1]
                     or (over._index[-1] == index[over_len-1]
                         and over._prec > self.prec)
                     or (over_len < len(index)
                         and over._index[-1] <= index[over_len-1]))):
                index[over_len-1] += 1

        elif isinstance(over, Remove):
            for idx in over._index_list:
                over_len = len(idx)
                if (over_len <= len(self._index)
                    and idx[:-1] == self._index[:over_len-1]):
                    if idx[-1] < self._index[over_len-1]:
                        index[over_len-1] -= 1
                    elif (over_len < len(self._index)
                          and idx[-1] == self._index[over_len-1]):
                        return NoOp(end_node, self._prec)

        elif isinstance(over, Move):
            # TODO: hackey, make this case work implicitly
            if over._index == over._dest_index:
                return Create(end_node, self._prec, index)

            src_len = len(over._index)
            moved = False
            if src_len <= len(index) and over._index[:-1] == index[:src_len-1]:
                if over._index[-1] == index[src_len-1] and src_len < len(index):
                    index[:src_len] = over._dest_index
                    moved = True
                elif over._index[-1] < index[src_len-1]:
                    index[src_len-1] -= 1

            dest_len = len(over._dest_index)
            if (not moved and dest_len <= len(index)
                and over._dest_index[:-1] == index[:dest_len-1]
                and (over._dest_index[-1] < index[dest_len-1]
                     or (over._dest_index[-1] == index[dest_len-1]
                         and (dest_len < len(index)
                              or over._prec > self.prec)))):
                index[dest_len-1] += 1

        return Create(end_node, self._prec, index)

    @property
    def tree_index(self):
        return self._index
