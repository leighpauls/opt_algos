from .. import Operation, Change, Printable
from client_node import ClientNode

class Client(Printable):
    """The main state representation of a client within the OPT system
    Attributes:
    value -- the active value of the tip of this client
    tip -- the ClientNode at the tip of this Client
    root -- the oldest historical node that the server could send changes relative to
    send_change_cb -- function to call when I want to send a change to the server, accepts a Change object
    local_change_q -- list of ClientNode object's whose local branch should be sent to the server
    pending_ack -- True iff I'm waitin for the server to ack my last local change before sending the next
    """
    def __init__(self, send_change_cb, init):
        """
        Params:
        send_change_cb -- The handler for local changes (Change) -> None
        init -- Initializer object sent from the Server
        """
        self.value = init.initial_value
        self.tip = self.root = ClientNode(server_state=init.initial_state,
                                          local_state=0)
        self.send_change_cb = send_change_cb
        self.local_change_q = []
        self.pending_ack = False
        self.prec = init.precedence

    def apply_local_change(self, operation, position, value):
        """Apply the change to the local value and enqueue it to send to the server
        Params:
        operation -- The op to apply
        postion -- enumerable position to apply it at
        value -- value to apply for an INSERT operation
        """
        new_tip = ClientNode(self.tip.server_state, self.tip.local_state + 1)
        new_edge = Operation(operation, position, value, new_tip, self.prec)

        old_tip = self.tip
        self.tip = old_tip.set_local_op(new_edge)
        self._apply_operation(new_edge)
        self._enqueue_local_change(old_tip)

    def apply_server_change(self, change):
        """Resolve the server change down to the tip and apply it to the local value
        Params:
        change -- The inbound Change object
        """
        # transform the change to the tip
        transform_source_node = self.root.transform_to_find(
            local_state=change.src_client_state,
            server_state=change.src_rel_server_state,
            root=self.root)

        raw_operation = Operation.from_server_change(
            change=change,
            source_node=transform_source_node)

        transform_source_node.set_server_op(raw_operation)

        # tranform down to the tip + 1 server change
        old_tip = self.tip
        self.tip = transform_source_node.transform_to_find(
            local_state=old_tip.local_state,
            server_state=transform_source_node.server_state + 1,
            root=self.root)

        self._apply_operation(old_tip.server_op)
        # no need to enqueue a remote change
    
    def apply_server_ack(self, ack):
        """Move the root so to forget about unneeded history objects
        Params:
        ack -- The Ack object sent from the server
        """
        if not self.pending_ack:
            raise "Got an ack while not pending on one"
        self.pending_ack = False

        # transform all pending changes to align on the
        # client axis of the new server state
        new_root = self.root.transform_to_find(local_state=ack.client_state,
                                               server_state=ack.rel_server_state,
                                               root=self.root)
        for i in range(0, len(self.local_change_q)):
            cur_node = self.local_change_q[i]
            self.local_change_q[i] = cur_node.transform_to_find(
                local_state=cur_node.local_state,
                server_state=new_root.server_state,
                root=self.root)

        self.root = new_root
        self._try_send_local_change()

    def _apply_operation(self, operation):
        if operation.op == Operation.INSERT:
            self.value.insert(operation.pos, operation.val)
        elif operation.op == Operation.DELETE:
            self.value.pop(operation.pos)
        elif operation.op == Operation.NO_OP:
            None
        else:
            raise "Unknwown operation: " + operation.op

    def _enqueue_local_change(self, source_node):
        self.local_change_q.append(source_node)
        self._try_send_local_change()

    def _try_send_local_change(self):
        if self.pending_ack or len(self.local_change_q) == 0:
            # can't send anything yet
            return
        self.pending_ack = True
        # I can assume at this point that the source node is the root history node
        source_node = self.local_change_q.pop(0)
        if source_node is not self.root:
            raise "trying to send a change from a point that may not be in the server's history"
        if source_node.local_op is None:
            raise "Sending something without an op"
        oper = source_node.local_op
        change = Change(src_client_state = source_node.local_state,
                        src_rel_server_state = source_node.server_state,
                        op = oper.op,
                        pos = oper.pos,
                        val = oper.val,
                        precedence = self.prec)
        self.send_change_cb(change)
