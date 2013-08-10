from operation import Insert, Delete, Create, Remove, Move

class CortexNode:
    def __init__(self, value=None, children=None, parent=None):
        self._value = value if value is not None else []
        self._children = children if children is not None else []
        self._parent = parent
        self._callback_map = {}

    def add_listener(self, operation_types, callback):
        for op_type in operation_types:
            op_name = op_type.OP_NAME
            if op_name in self._callback_map:
                self._callback_map[op_name] = [callback]
            else:
                self._callback_map[op_name].push(callback)

    def remove_listener(self, operation_types, callback):
        for op_type in operation_types:
            op_name = op_type.OP_NAME
            self._callback_map[op_name].remove(callback)

    def trigger_event(self, event):
        """Don't call this yourself, it should only be activated by one of the 
        local_op_* functions"""
        if event.OP_NAME not in self._callback_map:
            return None
        # call all the callbacks
        for callback in self._callback_map[event.OP_NAME]:
            callback(event)

    def is_equal(self, other):
        if (self._value != other._value 
            or len(self._children) != len(other._children)):
            return False
        for i in xrange(len(self._children)):
            if not self._children[i].is_equal(other._children[i]):
                return False
        return True

    def to_dict(self):
        return {
            "value": self._value,
            "children": [ch.to_dict() for ch in self._children]
            }

    @staticmethod
    def _from_dict_recurse(obj, parent):
        res = CortexNode(
            value=obj["value"],
            parent=parent)
        res.children = [
            CortexNode._from_dict_recurse(ch_obj, res)
            for ch_obj in obj["children"]]
        return res

    @staticmethod
    def from_dict(obj):
        return CortexNode._from_dict_recurse(obj, None)

    @property
    def children(self):
        return self._children
    @property
    def value(self):
        return self._value
    @property
    def parent(self):
        return self._parent

    def __str__(self):
        res = "<" + "".join(self._value) + ", ["
        for child in self._children:
            if child is self:
                raise Exception("Trying to print self as child")
            res += child.__str__() + ", "
        res += "]>"
        return res

    def get_child(self, child_index):
        return self._children[child_index]

    def insert_value(self, linear_index, value):
        if linear_index > len(self._value):
            raise Exception("Tried to insert value past the end of the array")
        self._value.insert(linear_index, value)

    def delete_value(self, linear_index):
        self._value.pop(linear_index)

    def create_child(self, child_index):
        if child_index > len(self._children):
            raise Exception("Tried to create child past the end of the array")
        self._children.insert(child_index, CortexNode(parent=self))
    
    def insert_child(self, child_index, child):
        if child_index > len(self._children):
            raise Exception("Tried to insert child past the end of the array")
        if child is self:
            raise Exception("Tried to add self as child")
        self._children.insert(child_index, child)
        child.set_parent(self)

    def set_parent(self, parent):
        self._parent = parent

    def pop_child(self, child_index):
        return self._children.pop(child_index)

    def remove_child(self, child_node):
        self._children.remove(child_node)

    def clone_tree(self, parent=None):
        new_children = [child.clone_tree(self) for child in self._children]
        new_value = self._value[:]
        return CortexNode(value=new_value,
                          children=new_children,
                          parent=parent)
    
    def _child_index(self, child):
        # TODO: fix this lookup
        return self._parent._child_index(self) + [self._children.index(child)]

    def _get_index(self):
        return self._parent._child_index(self)

    def _apply_local_change(self, operation_class, *op_args):
        self._parent._apply_local_change(operation_class, *op_args)
    
    # TODO: get rid of all the recursion done here
    # TODO: do operation validity checks here (eg, removing node [])
    # Local Commands, ie, allow the user to manipulate the tree

    def local_op_append_child(self):
        """Insert a new child at the end of this node's children"""
        insertion_index = self._get_index() + [len(self._children)]
        self._apply_local_change(Create, insertion_index)
    def local_op_prepend_child(self):
        """Insert a new Child at the start of this node's children"""
        insertion_index = self._get_index() + [0]
        self._apply_local_change(Create, insertion_index)
    def local_op_create_before(self):
        insertion_index = self._get_index()
        self._apply_local_change(Create, insertion_index)
    def local_op_create_after(self):
        insertion_index = self._get_index()
        insertion_index[-1] += 1
        self._apply_local_change(Create, insertion_index)

    def local_op_remove(self):
        """Remove this node"""
        self._apply_local_change(Remove, self._get_index())
        
    @staticmethod
    def __adjust_dest_idx(src_idx, cur_dest_idx):
        src_len = len(src_idx)
        dest_idx = cur_dest_idx[:]
        # if dest is a nefew of this node, adjust it properly
        if (src_len <= len(dest_idx)
            and src_idx[:-1] == dest_idx[:src_len-1]
            and src_idx[-1] < dest_idx[src_len - 1]):
            dest_idx[src_len-1] -= 1
        return dest_idx

    def local_op_move_after(self, after_node):
        """Move this node to be the sibling node after after_node"""
        src_idx = self._get_index()
        dest_idx = after_node._get_index()
        dest_idx[-1] += 1
        dest_idx = CortexNode.__adjust_dest_idx(
            src_idx,
            dest_idx)
        self._apply_local_change(
            Move,
            src_idx,
            dest_idx)

    def local_op_move_before(self, before_node):
        """Move this node to be the sibling node before to before_node"""
        src_idx = self._get_index()
        dest_idx = CortexNode.__adjust_dest_idx(
            src_idx,
            before_node._get_index())
        self._apply_local_change(
            Move,
            src_idx,
            dest_idx)

    def local_op_move_append(self, new_parent):
        """Move this node to be the last child of new_parent"""
        src_idx = self._get_index()
        dest_idx = CortexNode.__adjust_dest_idx(
            src_idx,
            new_parent._get_index() + [len(new_parent._children)])
        self._apply_local_change(
            Move,
            src_idx,
            dest_idx)

    def local_op_move_prepend(self, new_parent):
        """Move this node to be the first child of new_parent"""
        src_idx = self._get_index()
        dest_idx = CortexNode.__adjust_dest_idx(
            src_idx,
            new_parent._get_index() + [0])
        self._apply_local_change(
            Move,
            src_idx,
            dest_idx)

    def local_op_insert_char(self, index, val):
        """Insert 1 charactor at index in this node's value"""
        self._apply_local_change(Insert, self._get_index(), index, val)

    def local_op_delete_char(self, index):
        """remove 1 charactor at index in this node's value"""
        self._apply_local_change(Delete, self._get_index(), index)
