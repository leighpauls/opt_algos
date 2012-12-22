

from printable import Printable

class Server(Printable):
    """The main state representation of the Central OPt server
    Attributes:
    next_remote_id -- The next unique id number to assign a new remote to
    remotes -- a map of remote_id to the Remote object
    root -- the oldest ServerNode that I need to remember
    tip -- the ServerNode representing the current value
    value -- the current value of the tip entry
    """
    SERVER_HIDDEN_ID=1
    def __init__(self, initial_state=None, initial_value=None):
        self.next_remote_id = Server.SERVER_HIDDEN_ID + 1
        self.remotes = {}
        self.root = self.tip = ServerNode()
        self.root.append_state_axis(Server.SERVER_HIDDEN_ID)
        if initial_value is not None:
            self.root.increment_state(
                Server.SERVER_HIDDEN_ID,
                initial_state)
            self.value = initial_value
        else:
            self.value = []
        
    def _apply_change(self, change, remote_id):
        """Apply the change provided to the server state,
        then tell all of the remotes about it
        Params:
        change -- the Change object to apply
        remote_id -- the id number of the remote providing the change
        """
        # find the source state of the change
        source_node = self.root.find_source_of(change, remote_id)
        source_operation = operation.from_remote_change(change, source_node)

        # transform down to the tip
        new_tip_operation = source_node.transform_to_tip(source_operation, remote_id)
        self._apply_operation(new_tip_operation)
        new_tip = new_tip_operation.end
        
        # tell the remotes about the change
        for cur_remote_id, remote in self.remotes.iteritems():
            if cur_remote_id == remote_id:
                # TODO: see if I can move root to a newer node
                remote.server_ack_to_client(new_tip.make_ack(cur_remote_id))
            else:
                remote.server_change_available(self.tip.make_change(cur_remote_id))
        
        self.tip = new_tip

    def add_new_remote(self, handle_server_change, handle_ack_available):
        """Creates a new Remote controller for communication with a new Client
        Returns:
        A new Remote object to interface with a Client Object
        """
        new_remote_id = self.next_remote_id
        new_precedence = self.next_precedence
        self.next_remote_id += 1
        self.next_precedence += 1
        
        def on_remote_data(change):
            if change.prec != new_precedence:
                raise "Client tried to change it's own precedence"
            self._apply_change(change=change,
                              remote_id=new_remote_id)

        self.tip.append_state_axis(new_remote_id)

        new_remote = Remote(on_new_remote_change=on_remote_data,
                            on_new_server_change=handle_server_change,
                            on_ack_available=handle_ack_available,
                            Initializer(remote_id=new_remote_id,
                                        precedence=new_precedence,
                                        initial_value=self.value,
                                        initial_state=self._get_relative_state(new_remote_id)))
        self.remotes[new_remote_id] = new_remote
        return new_remote
