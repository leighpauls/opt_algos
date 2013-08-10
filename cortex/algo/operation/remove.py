from tree import Tree

class Remove(Tree):
    """Delete the node at _index"""

    OP_NAME="REMOVE"
    
    def __init__(self, end_node, prec, index, index_list=None):
        Tree.__init__(self, end_node, prec)
        if index_list is None:
            self._index_list = [index]
        else:
            self._index_list = index_list

    class Event(object):
        def __init__(self, old_node, parent):
            self.OP_NAME = Remove.OP_NAME
            self._old_node = old_node
            self._parent = parent
        @property
        def old_node(self):
            return self._old_node
        @property
        def parent(self):
            return self._parent

    @property
    def index_list(self):
        return self._index_list

    def apply(self, value_root):
        class Pair:
            pass
        remove_pairs = []
        # build a pointer-based deletion reference so sibling's can't screw with
        # eachother's indicies
        for index in self._index_list:
            pair = Pair();
            pair.parent = Tree._navigate_to_index_parent(index, value_root)
            pair.child = pair.parent.get_child(index[-1])
            remove_pairs.append(pair)
        for pair in remove_pairs:
            pair.parent.remove_child(pair.child)
            

    def _relocate_tree_index(self, old_index):
        # see if the node was removed, and find index changes
        new_index = old_index[:]
        old_idx_len = len(old_index)
        for removed_index in self._index_list:
            removed_idx_len = len(removed_index)
            if (len(old_index) >= removed_idx_len
                and old_index[:removed_idx_len-1] == removed_index[:-1]):
                if old_index[removed_idx_len-1] == removed_index[-1]:
                    return None
                elif old_index[removed_idx_len-1] >= removed_index[-1]:
                    new_index[removed_idx_len-1] -= 1
        return new_index

    def transform(self, over, end_node):
        from create import Create
        from move import Move
        
        index_list = []
        for idx in self._index_list:
            index_list.append(idx[:])

        if not isinstance(over, Tree):
            pass

        elif isinstance(over, Create):
            over_len = len(over._index)
            for idx in index_list:
                if (over_len <= len(idx)
                    and over._index[:-1] == idx[:over_len-1]
                    and over._index[-1] <= idx[over_len-1]):
                    idx[over_len-1] += 1
        elif isinstance(over, Remove):
            # look for common/redundant duplicates
            to_remove = []
            for over_idx in over._index_list:
                over_len = len(over_idx)
                for idx in index_list:
                    if over_len <= len(idx) and over_idx == idx[:over_len]:
                        to_remove.append(idx)
            for idx in to_remove:
                index_list.remove(idx)

            # sibling removal shifts
            for idx in index_list:
                # need to compare against the original idx, so save only the
                # deltas
                idx_len = len(idx)
                index_deltas = [0] * idx_len
                for over_idx in over._index_list:
                    over_len = len(over_idx)
                    if (over_len <= idx_len
                        and over_idx[:-1] == idx[:over_len-1]):
                        if over_idx[-1] < idx[over_len-1]:
                            index_deltas[over_len-1] -= 1
                        elif over_idx[-1] == idx[over_len-1]:
                            raise Exception("Missed a removal redundancy")
                # apply the deltas
                for i in xrange(idx_len):
                    idx[i] += index_deltas[i]


        elif isinstance(over, Move):
            # TODO: remove this whole implementation???
            src_len = len(over._index)
            dest_len = len(over._dest_index)

            for i in xrange(len(index_list)):
                idx = index_list[i]
                idx_len = len(idx)
                
                # removing child of source or source?
                if src_len <= idx_len and over._index == idx[:src_len]:
                    idx[:src_len] = over._dest_index
                else:
                    # removing parent of source?
                    if src_len > idx_len and over._index[:idx_len] == idx:
                        index_list.append(over._dest_index[:])

                    # removing sibling/child of move source?
                    if (src_len <= idx_len
                        and over._index[:-1] == idx[:src_len-1]
                        and over._index[-1] < idx[src_len-1]):
                        idx[src_len-1] -= 1
                    # removing sibling/child of move dest?
                    if (dest_len <= idx_len
                        and over._dest_index[:-1] == idx[:dest_len-1]
                        and over._dest_index[-1] <= idx[dest_len-1]):
                        idx[dest_len-1] += 1

        # remove redundant operations since they will cause errors
        redundant = []
        for i in xrange(len(index_list)):
            idx = index_list[i]
            if idx in redundant:
                continue
            for other_idx in index_list[i+1:]:
                if other_idx not in redundant:
                    if (len(idx) <= len(other_idx)
                        and idx == other_idx[:len(idx)]):
                        redundant.append(other_idx)
                    elif (len(idx) > len(other_idx)
                          and idx[:len(other_idx)] == other_idx):
                        redundant.append(idx)
                        break
        for r in redundant:
            index_list.remove(r)
                    
        return Remove(end_node, self._prec, index=None, index_list=index_list)
