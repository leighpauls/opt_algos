from ..operation import *

class ClientNode:
    """A single node in the client's history, an intersection between local and server ops
    Attribtues:
    server_state -- Number of the server change state
    local_state -- Number of the local change state
    local_op -- local Operation occuring after this node
    server_op -- server Operation occuring after this node
    """
    def __init__(self, server_state, local_state):
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
        # self._verify_xformable()

        if self.local_op.end.server_op is not None:
            transform_end_node = self.local_op.end.server_op.end
        else:
            transform_end_node = ClientNode(
                client_state=self.client_state + 1,
                server_state=self.server_state + 1)

        self.server_op.end.set_local_op(self.local_op.transform(
                over=self.server_op,
                end_node=transform_end_node))

    def _xform_server_over_local(self):
        """Transform this node's server operation over it's local operation,
        and apply it to the local_op end node"""
        # self._verify_xformable()

        if self.server_op.end.local_op is not None:
            transform_end_node = self.server_op.end.local_op.end
        else:
            transform_end_node = ClientNode(
                local_state=self.local_state + 1,
                server_state=self.server_state + 1)

        self.local_op.end.set_server_op(self.server_op.transform(
                over=self.local_op,
                end_node=transform_end_node))

    def to_csv_cell(self):
        return "(" + str(self._server_state) + " " + str(self._local_state) + ")"

    def dbg_try_all_paths(self, initial_value):
        """Recursively tries every operation path, to find logical xform errors"""
        try:
            if self.local_op is not None:
                local_copy = initial_value.clone_tree()
                self.local_op.apply(local_copy)
                self.local_op.end.dbg_try_all_paths(local_copy)
        except Exception:
            print "local (" + \
                str(self._server_state) + ", " + str(self._local_state) + ")"
            raise

        try:
            if self.server_op is not None:
                server_copy = initial_value.clone_tree()
                self.server_op.apply(server_copy)
                self.server_op.end.dbg_try_all_paths(server_copy)
        except Exception:
            print "srver (" + \
                str(self._server_state) + ", " + str(self._local_state) + ")"
            raise

    def dbg_verify_all_xforms(self, initial_value, server_age, local_age):
        """Evaluate every graph square to verify every xform pair"""
        evaluated = []
        for _ in xrange(server_age+1):
            new_col = []
            evaluated.append(new_col)
            for _ in xrange(local_age+1):
                new_col.append(None)
        evaluated[0][0] = initial_value.clone_tree()
        
        cur_server_state = 0
        cur_local_state = 0
        next_ss_start = 0
        cur_node = None
        next_row_node = self

        while next_ss_start is not None:
            cur_server_state = next_ss_start
            next_ss_start = None
            cur_node = next_row_node
            next_row_node = None
            
            done = False
            while not done:

                if cur_node.local_op is not None:
                    if next_ss_start is None:
                        next_ss_start = cur_server_state
                        next_row_node = cur_node.local_op.end

                    value_copy = evaluated[cur_server_state][cur_local_state].clone_tree()
                    cur_node.local_op.apply(value_copy)
                    if evaluated[cur_server_state][cur_local_state+1] is None:
                        evaluated[cur_server_state][cur_local_state+1] = value_copy
                    elif not evaluated[cur_server_state][cur_local_state+1].is_equal(value_copy):
                        raise Exception("Local op does not converge from (" 
                                        + str(cur_server_state) + ", " + str(cur_local_state) + ")")

                if cur_node.server_op is not None:
                    value_copy = evaluated[cur_server_state][cur_local_state].clone_tree()
                    cur_node.server_op.apply(value_copy)
                    if evaluated[cur_server_state+1][cur_local_state] is None:
                        evaluated[cur_server_state+1][cur_local_state] = value_copy
                    elif not evaluated[cur_server_state+1][cur_local_state].is_equal(value_copy):
                        print evaluated[cur_server_state+1][cur_local_state].__dict__
                        print value_copy.__dict__
                        raise Exception("Server op does not converge from (" 
                                        + str(cur_server_state) + ", " + str(cur_local_state) + ")")
                    cur_node = cur_node.server_op.end
                    cur_server_state += 1
                else:
                    done = True


            cur_local_state += 1
        print "Verified all xform pairs"
            

    def dump_csv(self):
        with open("node_dump.csv", "w") as f:
            # server accross, client down
            x = 0
            y = 0
            next_row_start = 0
            next_row_node = self
            passed = []

            while next_row_node is not None:
                cur_row = []
                local_row = []
                # add leading commas
                for i in xrange(0, next_row_start):
                    cur_row.append("")
                    cur_row.append("")
                    local_row.append("")
                    local_row.append("")
                cur_node = next_row_node
                next_row_node = None
                
                while cur_node is not None:
                    cur_row.append(cur_node.to_csv_cell())                        

                    if cur_node.local_op is not None:
                        local_row.append(cur_node.local_op.to_csv_cell())
                        if next_row_node is None:
                            next_row_node = cur_node.local_op.end
                            next_row_start = len(local_row)/2
                    else:
                        local_row.append("")
                        
                    local_row.append("")
                    
                    if cur_node.server_op is not None:
                        cur_row.append(cur_node.server_op.to_csv_cell())
                        cur_node = cur_node.server_op.end
                    else:
                        cur_node = None

                f.write("\t".join(cur_row) + "\n")
                f.write("\t".join(local_row) + "\n")
                
        
