from .. import Operation

class ClientNode:
    """A single node in the client's history, an intersection between local and server ops
    Attribtues:
    server_state -- Number of the server change state
    local_state -- Number of the local change state
    local_op -- local Operation occuring after this node
    server_op -- server Operation occuring after this node
    """
    def __init__(self, server_state, local_state):
        if type(server_state) is not int:
            raise "wrong state type"
        self._server_state = server_state
        self._local_state = local_state
        self.local_op = None
        self.server_op = None

    @property
    def server_state(self):
        return self._server_state
    @property
    def local_state(self):
        return self._local_state

    def set_local_op(self, op):
        if self.local_op is not None:
            raise "Tried to overwrite local op"
        self.local_op = op
    def set_server_op(self, op):
        if self.server_op is not None:
            raise "Tried to overwrite server op"
        self.server_op = op

    def transform_local_op(self, end_server_state, root):
        """Modify the state space until a local Operation leads out of (self.local_state, end_server_state)
        Assumes that there is a direct path from <root> to end_server_state along the server axis, and that 
        <root> is one client op older than self, and 0 or more server ops older
        Assumes that enough server state operations lead from root to get to <end_server_state>
        Params:
        end_server_state -- server state Number to transform the local op to
        root -- ClientNode gaurenteed to be older than self and the result
        Returns:
        The ClientNode holding the final transformed op
        """
        if end_server_state == self.server_state:
            # no need to transform
            return self

        if root.local_state != self.local_state - 1 or root.server_state > self.server_state:
            raise "Trying to use an invalid root"

        # transform down the root's server axis by building a line of "squares" 
        # from <root> to the end node diagonal
        cur_node = root
        while cur_node.server_state != end_server_state:
            # make the new local op
            if cur_node.server_op.end.local_op is None:
                cur_node._xform_local_over_server()
            # make the new server op
            if cur_node.local_op.end.server_op is None:
                cur_node._xform_server_op_over_local()
            cur_node = cur_node.server_op.end
        
        # transform self.local_op down to the end state
        cur_node = self
        while cur_node.server_state != end_server_state:
            if cur_node.server_op.end.local_op is None:
                cur_node._xform_local_over_server()
            cur_node = cur_node.server_op.end

        return cur_node

    def transform_server_op(self, end_local_state, root):
        """Modify the state space until a server operation leads out of (end_local_state, self.server_state)
        Assumes that the client has made enough changes to get to end_client_state
        Params:
        end_local_state -- The client state Number to transform down to
        root -- a ClientNode in the client axis of self, guaranteed to be older than/as old as any client operations in it's axis
        Returns:
        The Operation created at the end state
        """
        if root.local_state != self.local_state:
            raise "invalid root provided"
        if self.local_state > end_local_state:
            raise "tried to transform a server op backwards"

        # build a straight line of client operations from self to end_client_state
        # iterate down from root looking for client operations to transform down
        cur_node = root
        while cur_node.local_state != end_local_state:
            # look for a client op
            while cur_node.local_op is None:
                cur_node = cur_node.server_op.end

            # xform the client op down to my server state
            xforming_client_node = cur_node
            while xforming_client_node.server_state != self.server_state:
                if xforming_client_node.server_op.end.local_op is None:
                    xforming_client_node._xform_local_over_server()
                xforming_client_node = xforming_client_node.server_op.end

            cur_node = cur_node.local_op.end

        # xform self down the newly build line of local ops
        cur_node = self
        while cur_node.local_state != end_local_state:
            # no one else could have made these transforms yet
            cur_node._xform_server_over_local()
            cur_node = cur_node.local_op.end
            
        return cur_node.server_op

    def _verify_xformable(self):
        if self.local_op is None:
            raise "Tried to xform with no local op"
        if self.server_op is None:
            raise "Tried to xform with no server op"

    def _xform_local_over_server(self):
        """Transform this node's local operation over it's server operation,
        and apply it to the server_op end node"""
        self._verify_xformable()

        if self.local_op.end.server_op is not None:
            transform_end_node = self.local_op.end.server_op.end
        else:
            transform_end_node = ClientNode(
                client_state=self.client_state + 1,
                server_state=self.server_state + 1)

        self.server_op.end.set_local_op(Operation.transform(
                transform_op=self.local_op,
                over_op=self.server_op,
                end_node=transform_end_node))

    def _xform_server_over_local(self):
        """Transform this node's server operation over it's local operation,
        and apply it to the local_op end node"""
        self._verify_xformable()

        if self.server_op.end.local_op is not None:
            transform_end_node = self.server_op.end.local_op.end
        else:
            transform_end_node = ClientNode(
                local_state=self.local_state + 1,
                server_state=self.server_state + 1)

        self.local_op.end.set_server_op(Operation.transform(
                transform_op=self.server_op,
                over_op=self.local_op,
                end_node=transform_end_node))
        
