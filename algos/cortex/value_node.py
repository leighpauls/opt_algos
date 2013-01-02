
class ValueNode:
    def __init__(self, value=[], children=[]):
        self._value = value
        self._children = children

    def clone(self):
        return ValueNode(self._value[:], self._children[:])

    def get_child(self, child_index):
        return self._children[child_index]

    def insert_value(self, linear_index, value):
        self._value.insert(linear_index, value)

    def delete_value(self, linear_index):
        self._value.pop(linear_index)

    def create_child(self, child_index):
        self._children.insert(child_index, ValueNode())
    
    def insert_child(self, child_index, child):
        self._children.insert(child_index, child)

    def pop_child(self, child_index):
        return self._children.pop(child_index)
