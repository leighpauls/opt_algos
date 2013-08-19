
class Operator:
    """Translates structed operations into cortex local op calls
    Move operation indicies should not have post-operation adjustment"""
    
    def __init__(self, value):
        self._value = value

    def handle_operation(self, obj):
        obj_type = obj["type"]
        if obj_type == "insert":
            self._on_insert(obj)
        elif obj_type == "delete":
            self._on_delete(obj)
        elif obj_type == "append":
            self._on_append(obj)
        elif obj_type == "remove":
            self._on_remove(obj)
        elif obj_type == "move":
            self._on_move(obj)
        else:
            print "unknwon operation: " + data

    def _get_node(self, tree_idx):
        cur_node = self._value
        for idx in tree_idx:
            cur_node = cur_node.get_child(idx)
        return cur_node

    def _on_insert(self, obj):
        self._get_node(obj["tree_index"]).local_op_insert_char(
            obj["linear_index"],
            obj["value"])
    def _on_delete(self, obj):
        self._get_node(obj["tree_index"]).local_op_delete_char(
            obj["linear_index"])
    def _on_append(self, obj):
        self._get_node(obj["tree_index"]).local_op_append_child()
    def _on_remove(self, obj):
        self._get_node(obj["tree_index"]).local_op_remove()
    def _on_move(self, obj):
        dest_idx = obj["dest_tree_index"]
        new_parent = self._get_node(dest_idx[:-1])
        src_node = self._get_node(obj["src_tree_index"])
        src_node.local_op_move_to(new_parent, dest_idx[-1])
