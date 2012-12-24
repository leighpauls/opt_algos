from .. import Printable, Ack, Change
import copy

class ServerNode(Printable):
    """A state in the server's history
    Attributes:
    state -- the state vector at this node
    operation -- server.Operation away from this node
    """
    def __init__(self, old_node=None):
        if old_node is not None:
            self.state = copy.copy(old_node.state)
        else:
            self.state = {}
        self.operation = None

    def append_state_axis(self, client_id):
        if client_id in self.state:
            raise "Tried to append over an existing client axis"
        self.state[client_id] = 0

    def _increment_state(self, client_id):
        if client_id not in self.state:
            raise "Tried to increment a non-existant client axis"
        self.state[client_id] += 1

    def find_source_of(self, change, remote_id):
        """Finds the node in this list which was the souce of the given change
        Params:
        change -- a Change object to compare against
        remote_id -- the remote that the state is relative to
        Returns:
        The ServerNode which that change came from
        """
        cur_node = self
        while cur_node._get_client_state(remote_id) != change.src_client_state \
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
            transformed_op = operation.transform(
                transform_op = transformed_op,
                over_op = cur_node.operation,
                end_node = None)
        # cur_node is now the tip
        transformed_op.end = ServerNode(cur_node)
        transformed_op.end._increment_state(remote_id)
        
        cur_node.operation = transformed_op
        return transformed_op

    def make_ack(self, remote_id):
        """Make an Ack object for the remote that made this node"""
        return Ack(
            client_state=self._get_client_state(remote_id),
            rel_server_state=self.get_rel_server_state(remote_id))

    def make_change(self, remote_id):
        """Make a Change object which is consumable by the given remote object
        Call on the source of the operation in question.
        """
        return Change(
            src_client_state=self._get_client_state(remote_id),
            src_rel_server_state=self.get_rel_server_state(remote_id),
            op=self.operation.op,
            pos=self.operation.pos,
            val=self.operation.val,
            precedence=self.operation.prec)

    def get_rel_server_state(self, client_axis):
        # TODO: pre-cache the total age
        rel_server_state = 0
        for axis, state in self.state.iteritems():
            if axis != client_axis:
                rel_server_state += state
        return rel_server_state

    def _get_client_state(self, client_axis):
        if client_axis in self.state.keys():
            return self.state[client_axis]
        return 0
    
