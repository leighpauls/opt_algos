from client_node import ClientNode
from .. import Operation, Change

import copy

class Client:
    """The main state representation of a client within the OPT system
    Attributes:
    _value -- the active value of the tip of this client
    _tip -- the ClientNode at the tip of this Client
    _root -- the oldest historical node that the server could send changes relative to, where to send changes to the server from, and this will be in line of incomming server changes
    _local_change_q -- Q of ClientNodes whose local_ops should be sent to the server
    _send_change_cb -- function to call when I want to send a change to the server, (Change) -> void
    _pending_ack -- True iff I'm waitin for the server to ack my last local change before sending the next
    """
    def __init__(self, send_change_cb, init):
        """
        Params:
        send_change_cb -- The handler for local changes (Change) -> None
        init -- Initializer object sent from the Server
        """
        self._value = init.initial_value
        self._tip = self._root = ClientNode(server_state=init.initial_state,
                                            local_state=0)
        self._local_change_q = []
        self._send_change_cb = send_change_cb
        self._pending_ack = False
        self._prec = init.precedence

    def get_value(self):
        return copy.copy(self._value)
    
    def apply_local_change(self, operation, position, value):
        """Apply the change to the local value and enqueue it to send to the server
        Params:
        operation -- The op to apply
        postion -- enumerable position to apply it at
        value -- value to apply for an INSERT operation
        """
        self._apply_change(operation, position, value)
        new_tip = ClientNode(server_state=self._tip.server_state,
                             local_state=self._tip.local_state + 1)
        new_op = Operation(operation=operation,
                           position=position,
                           value=value,
                           end_node=new_tip,
                           precedence=self._prec)
        self._tip.set_local_op(new_op)
        old_tip = self._tip
        self._tip = new_tip
        self._local_change_q.append(old_tip)
        self._try_send_change()

    def apply_server_change(self, change):
        """Resolve the server change down to the tip and apply it to the local value
        Params:
        change -- The inbound Change object
        """
        if change.src_client_state != self._root.local_state:
            raise "Recieved server change which is not on server axis of root"
        # find the point in history where this change came from
        # TODO: keep a "server tip" pointer to skip this loop
        cur_node = self._root
        while cur_node.server_state != change.src_rel_server_state:
            cur_node = cur_node.server_op.end

        # transform the change down to the tip
        cur_node.set_server_op(Operation.from_server_change(change, cur_node))
        transformed_op = cur_node.transform_server_op(
            end_local_state=self._tip.local_state,
            root=self._root)

        # apply the transformed operation locally
        self._apply_change(operation=transformed_op.op,
                           position=transformed_op.pos,
                           value=transformed_op.val)
        old_tip = self._tip
        self._tip = transformed_op.end
        if self._root is old_tip:
            self._root = self._tip
        
    
    def apply_server_ack(self, ack):
        """Move the root so to forget about unneeded history objects
        Params:
        ack -- The Ack object sent from the server
        """
        if not self._pending_ack:
            raise "Got ack while not waiting for one"
        self._pending_ack = False

        self._local_change_q.pop(0)
        
        # transform the remaining local change q to the acked server state
        # I'm gaurenteed to have a straight line of history from root to (ack - 1 local)
        most_recent_root = self._root
        for i in range(len(self._local_change_q)):
            new_node = self._local_change_q[i].transform_local_op(
                end_server_state=ack.rel_server_state,
                root=most_recent_root)
            most_recent_root = self._local_change_q[i]
            self._local_change_q[i] = new_node

        # The start of the local change Q is now ensured to be the new root
        self._root = self._local_change_q[0] if len(self._local_change_q) > 0 else self._tip
        self._try_send_change()

    def _try_send_change(self):
        if self._pending_ack or len(self._local_change_q) == 0:
            # no change ready to send
            return

        new_change = Change(src_client_state=self._root.local_state,
                            src_rel_server_state=self._root.server_state,
                            op=self._root.local_op.op,
                            pos=self._root.local_op.pos,
                            val=self._root.local_op.val,
                            precedence=self._prec)
        self._pending_ack = True
        self._send_change_cb(new_change)
    
    def _apply_change(self, operation, position, value):
        """Apply the change to the local value"""
        if operation == Operation.INSERT:
            self._value.insert(position, value)
        elif operation == Operation.DELETE:
            self._value.pop(position)
        elif operation == Operation.NO_OP:
            None
        else:
            raise "unknown operation: " + operation
