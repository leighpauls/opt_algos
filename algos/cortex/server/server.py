from .. import Initializer
from ..operation.operation import Operation
from server_node import ServerNode
from remote import Remote
from state import State

import copy

class Server:
    """The main state representation of the Central OPt server
    Attributes:
    next_remote_id -- The next unique id number to assign a new remote to
    remotes -- a map of remote_id to the Remote object
    root -- the oldest ServerNode that I need to remember
    tip -- the ServerNode representing the current value
    value -- the current value of the tip entry
    """
    SERVER_HIDDEN_ID=1
    def __init__(self, initial_value=None):
        self.next_remote_id = Server.SERVER_HIDDEN_ID + 1
        self.next_precedence = 1
        self.remotes = {}
        self.root = self.tip = ServerNode()
        self.root.append_state_axis(Server.SERVER_HIDDEN_ID)
        self.value = copy.copy(initial_value) if initial_value is not None else []

    def _apply_change(self, change, remote_id):
        """Apply the change provided to the server state,
        then tell all of the remotes about it
        Params:
        change -- the Change object to apply
        remote_id -- the id number of the remote providing the change
        """
        # find the source state of the change
        source_node = self.root.find_source_of(change, remote_id)
        source_operation = Operation.from_change(change, None)

        # transform down to the tip
        new_tip_operation = source_node.transform_to_tip(source_operation, remote_id)
        new_tip_operation.apply(self.value)
        new_tip = new_tip_operation.end
        
        youngest_root = State(new_tip.pos)

        # tell the remotes about the change
        for cur_remote_id, remote in self.remotes.iteritems():
            if cur_remote_id == remote_id:
                remote.server_ack_to_client(new_tip.make_ack(cur_remote_id), new_tip.pos)
            else:
                remote.server_change_available(self.tip.make_change(cur_remote_id))
            youngest_root.age_to_include(remote.last_acked_state)

        self.tip = new_tip

        # see if I can move the root to a younger node
        while self.root.operation is not None and \
                not self.root.operation.end.pos.is_younger_than(youngest_root):
            self.root = self.root.operation.end


    def add_new_remote(self, handle_server_change, handle_ack_available):
        """Creates a new Remote controller for communication with a new Client
        Returns:
        A new Remote object to interface with a Client Object
        """
        new_remote_id = self.next_remote_id
        new_precedence = self.next_precedence
        self.next_remote_id += 1
        self.next_precedence += 1
        
        self.tip.append_state_axis(new_remote_id)

        initer = Initializer(
            remote_id=new_remote_id,
            precedence=new_precedence,
            initial_value=copy.copy(self.value),
            initial_state=self.tip.get_rel_server_state(new_remote_id))

        def on_remote_data(change):
            if change.prec != new_precedence:
                raise "Client tried to change it's own precedence"
            self._apply_change(change=change,
                               remote_id=new_remote_id)

        new_remote = Remote(
            on_new_remote_change=on_remote_data,
            on_new_server_change=handle_server_change,
            on_ack_available=handle_ack_available,
            initializer=initer,
            initial_state=self.tip.pos)
        self.remotes[new_remote_id] = new_remote
        return new_remote
