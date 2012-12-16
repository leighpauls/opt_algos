import random

from containers import *
from state import State
from remote import Remote

from copy import copy

class RemoteInitializer(Printable):
    """Serializable class used to initialize a new remote"""
    def __init__(self, server):
        self.value = list(server.value)
        self.age = server.state.age

class Server(Printable):
    """Represents a node in the OPT network.
    Attributes:
    value -- a local copy of the document being made
    state -- a state-space State of the remotes I know about
    remotes -- a map of key to Remote objects of the remotes I know about
    hist -- a list of the Hist history objects that I know about
    precedence --  a number which is unique to this node for resolving order-ambiguities
    """
    
    def __init__(self, origin_remote=None, remote_init=None):
        self.remotes = {}
        if origin_remote == None or remote_init == None:
            self.value = []
            self.state = State()
        else:
            self.value = list(remote_init.value)
            self.state = State(remote_init.age)
            self.add_remote(origin_remote)

        self.hist = []
        self.precedence = random.randint(0, 1 << 30)
        
    def add_remote(self, remote):
        """Add a new remote interface which will have new changes sent at it
        Attributes:
        remote -- Remote object to add
        """
        remote_key = self.state.add_remote()
        self.remotes[remote_key] = remote

        def apply_change_cb(remote_change):
            self._apply_remote_change(remote_key, remote_change)
        remote.set_remote_change_handler(apply_change_cb)

    def _apply_change(self, op):
        if op.op_type == OP_INSERT:
            self.value.insert(op.pos, op.val)
        elif op.op_type == OP_REMOVE:
            self.value.pop(op.pos)
        elif op.op_type == OP_NOOP:
            None
        else:
            raise "Invalid operation: " + op.op

    def _send_change_to_remote(self, remote_key, op):
        remote = self.remotes[remote_key]
        relative_source_state = self.state.get_relative_to_remote(remote_key)
        remote.enqueue_local_change(Change(
                relative_source_state[0], relative_source_state[1], op))

    def apply_local_change(self, op):
        """Make a change to the document on this node, and send out the changes.
        Attributes:
        op -- Operation to apply
        """
        self.hist.append(Hist(self.state.get_snapshot(), op, self.state.age))
        self._apply_change(op)

        # tell any clients about it
        for remote_key in self.remotes.keys():
            self._send_change_to_remote(remote_key, op)

        self.state.apply_local_change()
    

    def _apply_remote_change(self, source_remote_key, remote_change):
        transformed_op = self._transform_change(source_remote_key, remote_change)
        self.hist.append(Hist(self.state.get_snapshot(),
                              transformed_op,
                              self.state.age))
        
        self._apply_change(transformed_op)

        for remote_key, remote in self.remotes.items():
            if remote_key == source_remote_key:
                remote.send_remote_change_ack()
            else:
                self._send_change_to_remote(remote_key, transformed_op)

        self.state.apply_remote_change(source_remote_key)

    def _transform_change(self, remote_key, change):
        """Transform the change so that it can be applied on the local state.
        
        Attributes:
        remote_key -- key of the remote that made the change
        change -- Change object, from the perspective of that remote
        
        Returns:
        A Change object which is valid to apply on this server to bring self into
        the remote-dimention axis after the given change
        """
        cur_server, cur_remote = self.state.get_relative_to_remote(remote_key)
        if cur_server == change.server_src_state \
                and cur_remote == change.remote_src_state:
            # no need for a transform
            return change.op

        # find the changes that have happened since the change's state
        replay_ops = []
        for i in range(len(self.hist) - 1, 0, -1):
            entry = self.hist[i]
            replay_ops.append(entry.op)
            if entry.src_state[remote_key] == change.remote_src_state \
                    and entry.age - entry.src_state[remote_key] == \
                    change.server_src_state:
                break

        # replay the changes
        new_op = copy(change.op)
        for replay_op in replay_ops:
            if replay_op.op_type == OP_INSERT:
                if replay_op.pos < new_op.pos:
                    new_op.pos += 1
                elif replay_op.pos == new_op.pos \
                        and replay_op.precendece > new_op.precedence:
                    new_op.pos += 1
                    
            elif replay_op.op_type == OP_REMOVE:
                if replay_op.pos < new_op.pos:
                    new_op.pos -= 1
                elif new_op.op_type == OP_REMOVE and replay_op.pos == new_op.pos:
                    new_op.op_type = OP_NOOP
                    break
        return new_op
