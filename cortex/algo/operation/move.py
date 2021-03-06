from tree import Tree
from no_op import NoOp

class Move(Tree):
    """Move the node at _index to _dest_index
    Removes the node at _index, then re-inserts it at _dest_index after the
    later sister nodes have been shifted to fill the gap
    Attributes:
    _dest_index -- destination to insert the node moved from _index
    """

    OP_NAME="MOVE"
    
    def __init__(self, end_node, prec, index, dest_index):
        Tree.__init__(self, end_node, prec)
        self._index = index
        self._dest_index = dest_index

    class Event(object):
        def __init__(self, source_parent, moved_node):
            self.OP_NAME = Move.OP_NAME
            self._source_parent = source_parent
            self._moved_node = moved_node
        @property
        def source_parent(self):
            return self._source_parent
        @property
        def moved_node(self):
            return self._moved_node

    def apply(self, value_root):
        source_parent = Tree._navigate_to_index_parent(self._index, value_root)
        moved_node = source_parent.pop_child(self._index[-1])
        dest_parent = Tree._navigate_to_index_parent(
            self._dest_index,
            value_root)
        dest_parent.insert_child(self._dest_index[-1], moved_node)
        event = self.Event(source_parent, moved_node)
        source_parent.trigger_event(event)

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
        if (len(res) >= dest_idx_len
            and self._dest_index[:-1] == res[:dest_idx_len-1]):
            # treat it as an insert
            if self._dest_index[-1] <= res[dest_idx_len-1]:
                res[dest_idx_len-1] += 1
        return res

    @staticmethod
    def calc_effective_dest(src_idx, dest_idx):
        effective_dest = dest_idx[:]
        src_len = len(src_idx)
        if (src_len <= len(effective_dest)
            and src_idx[:-1] == effective_dest[:src_len-1]
            and (src_idx[-1] < effective_dest[src_len-1]
                 or (src_idx[-1] <= effective_dest[src_len-1]
                     and src_len < len(effective_dest)))):
            effective_dest[src_len-1] += 1
        return effective_dest
    
    @staticmethod
    def calc_operational_dest(own_src_idx, over_dest_idx):
        operational_dest = over_dest_idx[:]
        src_len = len(own_src_idx)
        if (src_len <= len(operational_dest)
            and own_src_idx[:-1] == operational_dest[:src_len-1]
            and (own_src_idx[-1] < operational_dest[src_len-1]
                 or (own_src_idx[-1] <= operational_dest[src_len-1]
                     and src_len < len(operational_dest)))):
            operational_dest[src_len-1] -= 1
        return operational_dest
    
    @staticmethod
    def is_child_of(parent_idx, child_idx):
        """Returns true iff <parent_idx> is a parent of <child_idx>"""
        return (len(parent_idx) < len(child_idx)
                and parent_idx == child_idx[:len(parent_idx)])

    def transform(self, over, end_node):
        from create import Create
        from remove import Remove
        from move import Move

        src_index = self._index[:]
        dest_index = self._dest_index[:]

        if not isinstance(over, Tree):
            pass

        elif isinstance(over, Create):
            effective_dest = Move.calc_effective_dest(src_index, dest_index)
            over_len = len(over._index)
            if (over_len <= len(src_index)
                and over._index[:-1] == src_index[:over_len-1]
                and over._index[-1] <= src_index[over_len-1]):
                src_index[over_len-1] += 1

            if (over_len <= len(effective_dest)
                and over._index[:-1] == effective_dest[:over_len-1]
                and (over._index[-1] < effective_dest[over_len-1]
                     or (over._index[-1] == effective_dest[over_len-1]
                         and (over._prec > self.prec
                              or self._index == self._dest_index))
                     or (over_len < len(effective_dest)
                         and over._index[-1] <= effective_dest[over_len-1]))):
                dest_index[over_len-1] += 1

        elif isinstance(over, Remove):

            effective_dest = Move.calc_effective_dest(src_index, dest_index)
            become_remove_source = False
            
            for over_idx in over._index_list:
                over_len = len(over_idx)
                if (len(over_idx) <= len(self._index)
                    and over_idx == self._index[:over_len]):
                    return NoOp(end_node, self._prec)

                elif (len(over_idx) < len(effective_dest)
                      and over_idx == effective_dest[:over_len]):
                    become_remove_source = True

                if (len(over_idx) <= len(self._index)
                    and over_idx[:-1] == self._index[:over_len-1]
                    and over_idx[-1] < self._index[over_len-1]):
                    src_index[over_len-1] -= 1
                if (len(over_idx) <= len(effective_dest)
                    and over_idx[:-1] == effective_dest[:over_len-1]
                    and over_idx[-1] < effective_dest[over_len-1]):
                    dest_index[over_len-1] -= 1

            if become_remove_source:
                return Remove(end_node, self._prec, src_index)
                

        elif isinstance(over, Move):
            # can't do an xform if we both try to move the same node, so use
            # precedence and NoOp
            if over._index == src_index:
                if over._prec > self._prec:
                    return NoOp(end_node, self._prec)
                else:
                    return Move(
                        end_node,
                        self._prec,
                        over._dest_index[:],
                        dest_index)

            # HACK: don't try to xform over/with a pointless move, it just makes
            # the logic suck
            if over._index == over._dest_index:
                return Move(end_node, self._prec, src_index, dest_index)
            if src_index == dest_index:
                return NoOp(end_node, self._prec)

            my_effective_dest = Move.calc_effective_dest(
                self._index,
                self._dest_index)
            over_effective_dest = Move.calc_effective_dest(
                over._index,
                over._dest_index)
            # TODO: HACK: undo cyclical dependencies
            if (Move.is_child_of(over._index, my_effective_dest)
                and Move.is_child_of(self._index, over_effective_dest)):
                # just undo over's operation
                src_index = over._dest_index[:]
                dest_index = over._index[:]
                return Move(end_node, self._prec, src_index, dest_index)

            over_src_len = len(over._index)
            over_dest_len = len(over._dest_index)
            my_src_len = len(src_index)

            # move my src idx
            src_moved = False
            if (over_src_len <= my_src_len
                and over._index[:-1] == src_index[:over_src_len-1]):
                if over._index[-1] == src_index[over_src_len-1]:
                    src_moved = True
                    src_index[:over_src_len] = over._dest_index
                elif over._index[-1] < src_index[over_src_len-1]:
                    src_index[over_src_len-1] -= 1
            if (not src_moved and over_dest_len <= my_src_len
                and over._dest_index[:-1] == src_index[:over_dest_len-1]
                and over._dest_index[-1] <= src_index[over_dest_len-1]):
                src_index[over_dest_len-1] += 1

            my_dest_len = len(my_effective_dest)            

            dest_moved = False
            if (over_src_len <= my_dest_len
                and over._index[:-1] == my_effective_dest[:over_src_len-1]):
                if (over._index[-1] == my_effective_dest[over_src_len-1]
                    and over_src_len < my_dest_len):
                    dest_moved = True
                    dest_index[:over_src_len] = Move.calc_operational_dest(
                        src_index,
                        over._dest_index)
                elif over._index[-1] < my_effective_dest[over_src_len-1]:
                    dest_index[over_src_len-1] -= 1
                    my_effective_dest[over_src_len-1] -= 1

            if (not dest_moved and over_dest_len <= my_dest_len
                and over._dest_index[:-1] == my_effective_dest[:over_dest_len-1]
                and (over._dest_index[-1] < my_effective_dest[over_dest_len-1]
                     or (over._dest_index == my_effective_dest
                         and over._prec > self._prec)
                     or (len(over._dest_index) < len(my_effective_dest)
                         and (over._dest_index[-1]
                              <= my_effective_dest[over_dest_len-1])))):
                dest_index[over_dest_len-1] += 1

        return Move(end_node, self._prec, src_index, dest_index)

    @property
    def dest_tree_index(self):
        return self._dest_index

    @property
    def tree_index(self):
        return self._index
