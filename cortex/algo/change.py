class Change:
    """Represents a change, readable by both Server and Client
    Attributes:
    src_clinet_state -- The source state as understood by the client
    sending/recieveing this message
    src_server_state -- The source state of the server, relative to the client
    state sending/recieving this message
    op -- The operation identifier
    tree_index -- index to the node to apply the change
    linear_index -- value index to apply at
    val -- value for an INSERT operation
    prec -- the precedence of the server that made the change
    """
    def __init__(
        self,
        src_client_state,
        src_rel_server_state,
        op,
        tree_index,
        dest_tree_index,
        linear_index,
        val,
        precedence,
        index_list):
        self.src_client_state = src_client_state
        self.src_rel_server_state = src_rel_server_state
        self.op = op
        self.tree_index = tree_index
        self.dest_tree_index = dest_tree_index
        self.linear_index = linear_index
        self.val = val
        self.prec = precedence
        self.index_list = index_list
        
    def to_dict(self):
        return {
            "src_client_state": self.src_client_state,
            "src_rel_server_state": self.src_rel_server_state,
            "op": self.op,
            "tree_index": self.tree_index,
            "dest_tree_index": self.dest_tree_index,
            "linear_index": self.linear_index,
            "val": self.val,
            "prec": self.prec,
            "index_list": self.index_list}
        
    @staticmethod
    def from_dict(obj):
        return Change(
            obj["src_client_state"],
            obj["src_rel_server_state"],
            obj["op"],
            obj["tree_index"],
            obj["dest_tree_index"],
            obj["linear_index"],
            obj["val"],
            obj["prec"],
            obj["index_list"])
    
