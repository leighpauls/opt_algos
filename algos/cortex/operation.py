
def abstract(init_func, abstract_type):
    def _checker(self, *args):
        if self.__class__ is abstract_type:
            raise "Tried to instantiate abstract type:", abstract_type
        init_func(self, *args)
    return _checker

class Operation:

    @abstract(Operation)
    def __init__(self, end_node, prec):
        self._end_node = end_node
        self._prec = prec

    class NoOp(Operation):
        def __init__(self, end_node, prec):
            super(NoOp, self).__init__(end_node, prec)
            
            def apply(self, value_tree): pass
            def transform(self, over, end_node):
                return NoOp(end_node)

    class Value(Operation):
        """Operations modifying the value of a node
        Attributes:
        _tree_index -- index list to the node being modified
        _linear_index -- location in that node's value being modified
        """
        @abstract(Value)
        def __init__(self, end_node, tree_index, linear_index):
            super(Value, self).__init__(end_node)
            self._tree_index = tree_index
            self._linear_index = linear_index

        def _navigate_to_tree_node(self, value_root):
            cur_node = value_root
            for index in self._tree_index:
                cur_node = cur_node.get_child(index)
            return cur_node

        def _get_xformed_tree_index(self, over):
            if not isinstance(over, Operation.Tree):
                return self._tree_index[:]
            return over._relocate_tree_index(self._tree_index)
                

        class Insert(Value):
            """Insert Value Operation
            Attributes:
            value -- the value to insert at _tree_index,_linear_index
            """
            def __init__(self, end_node, tree_index, linear_index, value):
                super(Insert, self).__init__(end_node, tree_index, linear_index)
                self._value = value

            def apply(self, value_root):
                node = self._navigate_to_tree_node(value_root)
                node.insert_value(self._linear_index, self._value)

            def transform(self, over, end_node):
                # find the tree node to apply to
                tree_index = self._get_xformed_tree_index(over)
                if tree_index is None:
                    return Operation.NoOp(end_node)

                # find the location in the value to apply to
                linear_index = self._linear_index
                if isinstance(over, Value.Insert):
                    if over._linear_index < linear_index or (
                        over._linear_index == lienar_index and over._prec > self.prec):
                        linear_index += 1
                elif isinstance(over, Value.Delete):
                    if over._linear_index < linear_index:
                        lienar_index -= 1

                return Insert(end_node, tree_index, linear_index, self._value)
                
        class Delete(Value):
            """Delete Value Operation, deletes value at _tree_index,_linear_index"""
            def __init__(self, end_node, tree_index, linear_index):
                super(Delete, self).__init__(end_node, tree_index, linear_index)

            def apply(self, value_root):
                node = self._navigate_to_tree_node(value_root)
                node.delete_value(self._linear_index)

            def transfrom(self, over, end_node):
                # find the tree node to apply to
                tree_index = self.get_xformed_tree_index(over)
                if tree_index is None:
                    return Operation.NoOp(end_node)
                
                # find where in the value to apply the delete
                linear_index = self._linear_index
                if isinstance(over, Value.Insert):
                    if over._linear_index <= linear_index:
                        linear_index += 1
                elif isinstance(over, Value.Delete):
                    if over._linear_index < linear_index:
                        linear_index -= 1
                    elif over._linear_index == linear_index:
                        return Operation.NoOp(end_node)

                return Value.Delete(end_node, tree_index, linear_index)

    class Tree(Operation):
        """Operations modifying the tree structure of the document
        Attributes:
        _index -- list of child indicies to apply the operation at
        """
        @abstract(Tree)
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

        class Create(Tree):
            """Create a new node at _index"""
            def __init__(self, end_node, index):
                super(Insert, self).__init__(end_node, index)

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

                elif isinstance(over, Tree.Insert):
                    over_len = len(over._index)
                    if over_len <= len(index) and over._index[:-1] == index[:over_len-1] \
                            and (over._index[-1] < index[over_len-1] or (over._index[-1] == index[over_len-1]
                                                                         and over._prec > self.prec)):
                        index[over_len-1] += 1

                elif isinstance(over, Tree.Delete):
                    over_len = len(over._index)
                    if over_len <= len(index) and over._index[:-1] == index[:over_len-1] \
                            and over._index[-1] < index[over_len-1]:
                        index[over_len-1] -= 1

                elif isinstance(over, Tree.Move):
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
                
        class Delete(Tree):
            """Delete the node at _index"""
            def __init__(self, end_node, index):
                super(Insert, self).__init__(end_node, index)

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

                elif isinstance(over, Tree.Delete):
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
                
        class Move(Tree):
            """Move the node at _index to _dest_index
            Attributes:
            _dest_index -- destination to insert the node moved from _index
            """
            def __init__(self, end_node, index, dest_index):
                super(Move, self).__init__(end_node, index)
                self._dest_index = dest_index

            def apply(self, value_root):
                source_parent = Tree._navigate_to_index_parent(self._index, value_root)
                dest_parent = Tree._nacigate_to_index_parent(self._dest_index, value_root)
                
                dest_parent.add_child(self._dest_index[-1], source_parent.pop_child(self._index[-1]))

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
                src_index = self._index[:]
                dest_index = self._dest_index[:]
                if not isinstance(over, Tree):
                    pass
                
                elif isinstance(over, Tree.Insert):
                    over_len = len(over._index)
                    if over_len <= len(src_index) and over._index[:-1] == src_index[:over_len-1] \
                            and over._index[-1] <= src_index[over_len-1]:
                        src_index[over_len-1] += 1
                    
                    if over_len <= len(dest_index) and over._index[:-1] == dest_index[:over_len-1] \
                            and (over._index[-1] < dest_index[over_len-1]
                                 or (over._index[-1] == dest_index[over_len-1] and over._prec > self.prec)):
                        dest_index[over_len-1] += 1

                elif isinstance(over, Tree.Delete):
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
                    # TODO: move my dest index according to over's source index?
                    # TODO: move my dest index according to over's dest index?
                    
