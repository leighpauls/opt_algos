import random

from containers import *
from state import State
from remote import Remote

class RemoteInitializer(Printable):
    def __init__(self, server):
        self.value = server.value
        self.age = server.state.age

class Server(Printable):
    def __init__(self, origin_remote=None, remote_init=None):
        if origin_remote == None or remote_init == None:
            self.value = []
            self.state = State()
            self.remotes = {}
        else:
            self.value = remote_init.value.copy()
            self.state = State(age)
            remote_key = self.state.add_remote()
            self.remotes = { remote_key: origin_remote }

        self.hist = []
        self.precedence = random.randint(0, 1 << 30)
        
    def add_remote(self, remote):
        remote_key = self.state.add_remote()
        self.remotes[remote_key] = remote

        def apply_change_cb(remote_change):
            self.apply_remote_change(remote_key, remote_change)
        remote.set_remote_change_handler(apply_change_cb)
        remote.init_remote_cb(RemoteInitializer(self))

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
        self.hist.append(Hist(self.state.get_snapshot(), op, self.state.age))
        self._apply_change(op)

        # tell any clients about it
        for remote_key in self.remotes.keys():
            self._send_change_to_remote(remote_key, op)

        self.state.apply_local_change()
    

    def apply_remote_change(self, source_remote_key, remote_change):
        transformed_op = self.transform_change(source_remote_key, remote_change)
        self.hist.append(Hist(self.state.get_snapshot(),
                              transformed_op,
                              self.state.age))
        
        self._apply_change(transformed_op)

        for remote_key, remote in self.remotes:
            if remote_key == source_remote_key:
                remote.ack_remote_change_cb()
            else:
                self._send_change_to_remote(remote_key, transformed_op)

        self.state.apply_remote_change(source_remote_key)

    def transform_change(remote_key, change):
        cur_server, cur_remote = self.state.get_relative_to_remote(remote_key)
        if cur_server == change.server_src_state \
                and cur_remote == change.remote_src_state:
            # no need for a transform
            return change.op

        # find the changes that have happened since the change's state
        replay_ops = []
        for i in range(len(hist) - 1, 0, -1):
            entry = self.hist[i]
            replay_ops.append(entry.op)
            if entry.src_state[remote_key] == change.remote_src_state \
                    and entry.age - entry.src_state[remote_key] == \
                    change.server_src_state:
                break

        # replay the changes
        new_op = remote_change.op.copy()
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
