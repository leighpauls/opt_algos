from .. import Ack, Change
from ..operation.operation import Operation
from state import State

class ServerNode:
    """A state and transition in the server's history
    Attributes:
    pos -- the State Object representing this node in state-space
    operation -- server.Operation away from this node
    """
    def __init__(self, old_node=None):
        if old_node is not None:
            self.pos = State(old_node.pos)
        else:
            self.pos = State()
        self.operation = None

    def append_state_axis(self, client_id):
        self.pos.append_state_axis(client_id)

    def find_source_of(self, change, remote_id):
        """Finds the node in this list which was the souce of the given change
        Params:
        change -- a Change object to compare against
        remote_id -- the remote that the state is relative to
        Returns:
        The ServerNode which that change came from
        """
        cur_node = self
        while cur_node.pos.get_axis_state(remote_id) != change.src_client_state \
                or cur_node.get_rel_server_state(remote_id) != change.src_rel_server_state:
            if cur_node.operation is None:
                raise "The change did not come from after this node"
            cur_node = cur_node.operation.end
        return cur_node

    def transform_to_tip(self, source_operation, remote_id):
        """Transforms the operation from this node to the tip, then uses it to make a new tip
        Params:
        source_operation -- an operation whose source is this node
        remote_id -- the id Number of the remote that caused the change
        Returns:
        The operation pointing to the new tip of this list
        """
        cur_node = self
        transformed_op = source_operation
        while cur_node.operation is not None:
            transformed_op = transformed_op.transform(
                end_node=None,
                over=cur_node.operation)
            cur_node = cur_node.operation.end
        # cur_node is now the tip
        transformed_op.end = ServerNode(cur_node)
        transformed_op.end.pos.increment_state(remote_id)
        
        cur_node.operation = transformed_op
        return transformed_op

    def make_ack(self, remote_id):
        """Make an Ack object for the remote that made this node"""
        return Ack(
            client_state=self.pos.get_axis_state(remote_id),
            rel_server_state=self.get_rel_server_state(remote_id))

    def make_change(self, remote_id):
        """Make a Change object which is consumable by the given remote object
        Call on the source of the operation in question.
        """
        return Change(
            src_client_state=self.pos.get_axis_state(remote_id),
            src_rel_server_state=self.get_rel_server_state(remote_id),
            op=self.operation.op,
            tree_index=self.operation.tree_index,
            dest_tree_index=self.operation.dest_tree_index,
            linear_index=self.operation.linear_index,
            val=self.operation.val,
            precedence=self.operation.prec)

    def get_rel_server_state(self, remote_id):
        return self.pos.get_relative_state(remote_id)
    
