import operation
from printable import Printable

class Node(Printable):
    """One state in the history graph
    Attributes:
    server_state -- The server's independant state (# applied changes) relative to this client at this node
    local_state -- # applied local changes to get to this state
    local_op -- The next local operation from this state
    server_op -- The next remote operation from this state
    """
    def __init__(self, server_state, local_state):
        self.server_state = server_state
        self.local_state = local_state
        self.local_op = None
        self.server_op = None
    def set_local_op(self, op):
        if self.local_op is not None:
            raise "Tried to reset a local op"
        self.local_op = op
        return op.end
    def set_server_op(self, op):
        if self.server_op is not None:
            raise "Tried to reset a server op"
        self.server_op = op
        return op.end
    def transform_to_find(self, local_state, server_state, root):
        """Find the given state in the state space below this node
        Will execute transformations to get there if needed
        Params:
        local_state -- state value (counter) of the client to find
        server_satte -- state value (counter) of the server to find
        root -- A Node gaurenteed to be above both this node and the node I'm tying to find
        """
        if local_state < self.local_state:
            raise "Tried to find a local state older than myself"
        if server_state < self.server_state:
            raise "Tried to find a server state older than myself"
        res = self

        while res.local_state != local_state or res.server_state != server_state:
            if res.local_state < local_state:
                if res.local_op is not None:
                    res = res.local_op.end
                elif res.server_state < server_state and res.server_op is not None:
                    res = res.server_op.end
                else:
                    res = res._transform_to_local(root).end
            elif res.server_state < server_state:
                if res.server_op is not None:
                    res = res.server_op.end
                else:
                    res = res._transform_to_server(root).end

        return res

    def _transform_to_server(self, root):
        raise "NotImpl"

    def _transform_to_local(self, root):
        """Transform to the local node and return it"""
        if self.local_op is not None:
            raise "Tried to transform over a existant local node"
        root._execute_local_transformations_to_node(self)
        return self.local_op

    def _execute_local_transformations_to_node(self, source_node):
        """Transform until the node one more local than source node has been made
        Should be executed from the root node
        Params:
        source_node -- The node that the resulting op should applied to"""
        # find an existing node with the same local state as the source
        cur_node = self
        while cur_node.local_state != source_node.local_state:
            if cur_node.local_op is not None:
                cur_node = cur_node.local_op.end
            elif cur_node.server_op is not None:
                cur_node = cur_node.server_op.end
            else:
                raise "Got to a dead end looking for a transformation seed"
        # look for the last node with this local state to have a local op
        while cur_node.server_op is not None \
                and cur_node.server_op.end.local_op is not None:
            cur_node = cur_node.server_op.end
        # transform over server operations until we've
        #  transformed on to the source node
        while cur_node is not source_node:
            # tranformation logic
            # make a blank node if it doesn't already exist
            if cur_node.local_op.end.server_op is not None:
                end_node = cur_node.local_op.end.server_op.end
            else:
                end_node = Node(local_state=cur_node.local_state + 1,
                                server_state=cur_node.server_state + 1)

            new_op = operation.Operation.transform(tranform_op=cur_node.local_op,
                                         over_op=cur_node.server_op,
                                         end_node=end_node)
            cur_node.server_op.end.local_op = new_op
            cur_node = cur_node.server_op.end
