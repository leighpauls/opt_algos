
OP_INSERT = "insert"
OP_REMOVE = "remove"
OP_NOOP = "noop"

class Printable:
    def __repr__(self):
        return self.__str__()
    def __str__(self):
        res = ["<", str(self.__class__), ':\n']
        for k, v in self.__dict__.items():
            res += ['  ', k, ": ", str(v).replace('\n', '\n  '), ',\n']
        res += [">"]
        return "".join(res)

class Operation(Printable):
    def __init__(self, op_type, pos, val):
        self.op_type = op_type
        self.pos = pos
        self.val = val
class Hist(Printable):
    def __init__(self, src_state, op, age):
        self.src_state = src_state
        self.op = op
        self.age = age
class Change(Printable):
    def __init__(self, server_src_state, remote_src_state, op):
        self.server_src_state = server_src_state
        self.remote_src_state = remote_src_state
        self.op = op

class State(Printable):
    _INTERNAL_KEY = 0
    def __init__(self):
        self.val = { State._INTERNAL_KEY: 0 }
        self.next_key = State._INTERNAL_KEY + 1
        self.age = 0
    def add_remote(self):
        res = self.next_key
        self.val[res] = 0
        self.next_key += 1
        return res
    def apply_local_change(self):
        self.val[State._INTERNAL_KEY] += 1
        self.age += 1
    def apply_remote_change(self, remote):
        self.val[remote] += 1
        self.age += 1
    def get_snapshot(self):
        return self.val.copy()
    def get_relative_to_remote(self, remote_key):
        remote_state = self.val[remote_key]
        return (self.age - remote_state, remote_state)

class Remote(Printable):
    def __init__(self, local_change_cb, ack_remote_change_cb):
        self.local_change_cb = local_change_cb
        self.ack_remote_change_cb = ack_remote_change_cb
        self.local_change_queue = []
        self.wait_for_ack = False
        self.remote_change_handler = None
        self.remote_change_queue = []

    def handle_remote_change(self, remote_change):
        if not self.remote_change_handler == None:
            self.remote_change_handler(remote_change)
        else:
            self.remote_change_queue.append(remote_change)

    def set_remote_change_handler(self, cb):
        self.remote_change_handler = cb
        if not self.remote_change_queue == None:
            for remote_change in self.remote_change_queue:
                self.remote_change_handler(remote_change)
        remote_change_queue = None
        
    def try_send_queue(self):
        if self.wait_for_ack or len(self.local_change_queue) == 0:
            return
        self.local_change_cb(self.local_change_queue)
        self.local_change_queue = []
        self.wait_for_ack = True

    def enqueue_local_change(self, change):
        self.local_change_queue.append(change)
        self.try_send_queue()

    def handle_remote_ack(self):
        self.wait_for_ack = False
        self.try_send_queue()


class Server(Printable):
    def __init__(self):
        self.value = []
        self.hist = []
        self.state = State()
        self.remotes = {}
        self.precedence = random.randint(0, 1 << 30)
        
    def add_remote(self, remote):
        remote_key = self.state.add_remote()
        self.remotes[remote_key] = remote

        def apply_change_cb(remote_change):
            self.apply_remote_change(remote_key, remote_change)
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
            elif replay_op.op_type == OP_REMOVE:
                if replay_op.pos < new_op.pos:
                    new_op.pos -= 1
                elif new_op.op_type == OP_REMOVE and replay_op.pos == new_op.pos:
                    new_op.op_type = OP_NOOP
                    break
        return new_op

def make_printer(label):
    def func(a):
        print label, ": ", str(a)
    return func



def main():
    s = Server()
    r1 = Remote(make_printer("r1 Local"), make_printer("r1 ACK"))
    r2 = Remote(make_printer("r2 Local"), make_printer("r2 ACK"))
    s.add_remote(r1)
    s.add_remote(r2)
    
    s.apply_local_change(Operation(OP_INSERT, 0, "a"))
    s.apply_local_change(Operation(OP_INSERT, 1, "b"))

    r1.handle_remote_ack()
    r2.handle_remote_ack()

    print s
    


if __name__ == "__main__":
    main()
