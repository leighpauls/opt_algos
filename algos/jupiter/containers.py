
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
    def __init__(self, op_type, pos, val, precedence):
        self.op_type = op_type
        self.pos = pos
        self.val = val
        self.precedence = precedence
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
