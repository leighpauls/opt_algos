
class CortexNode:
    def __init__(self, value=None, children=None):
        self._value = value if value is not None else []
        self._children = children if children is not None else []

    def is_equal(self, other):
        if self._value != other._value or len(self._children) != len(other._children):
            return False
        for i in xrange(len(self._children)):
            if not self._children[i].is_equal(other._children[i]):
                return False
        return True

    # TODO: remove these debug properties
    @property
    def children(self):
        return self._children
    @property
    def value(self):
        return self._value

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
        self._children.insert(child_index, CortexNode())
    
    def insert_child(self, child_index, child):
        if child_index > len(self._children):
            raise Exception("Tried to insert child past the end of the array")
        if child is self:
            raise Exception("Tried to add self as child")
        self._children.insert(child_index, child)

    def pop_child(self, child_index):
        return self._children.pop(child_index)

    def remove_child(self, child_node):
        self._children.remove(child_node)

    def clone_tree(self):
        new_children = [child.clone_tree() for child in self._children]
        new_value = self._value[:]
        return CortexNode(value=new_value,
                         children=new_children)
