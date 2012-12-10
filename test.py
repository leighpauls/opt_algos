
import algos.jupiter as j

def make_printer(label):
    def func(a):
        print label, ": ", str(a)
    return func

def main():
    s = j.Server()

    def make_assigner(var):
        def func(a):
            var = a
        return func

    state_1 = None
    state_2 = None

    r1 = j.Remote(make_printer("r1 Local"), make_printer("r1 ACK"), make_assigner(state_1))
    r2 = j.Remote(make_printer("r2 Local"), make_printer("r2 ACK"), make_assigner(state_2))
    s.add_remote(r1)
    s.add_remote(r2)
    
    s.apply_local_change(j.Operation(j.OP_INSERT, 0, "a", s.precedence))
    s.apply_local_change(j.Operation(j.OP_INSERT, 1, "b", s.precedence))

    r1.handle_remote_ack()
    r2.handle_remote_ack()

    print s


if __name__ == "__main__":
    main()
