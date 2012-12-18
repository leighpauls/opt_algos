
import algos.jupiter as jup

def make_printer(label):
    def func(a):
        print label, ": ", str(a)
    return func

def simple_test():
    s = jup.Server()

    r1 = jup.Remote()
    r2 = jup.Remote()

    r1.set_local_change_cb(make_printer("r1 Local"))
    r1.set_ack_remote_change_cb(make_printer("r1 ACK"))
    r2.set_local_change_cb(make_printer("r2 Local"))
    r2.set_ack_remote_change_cb(make_printer("r2 ACK"))
    

    s.add_remote(r1)
    s.add_remote(r2)
    
    s.apply_local_change(jup.Operation(jup.OP_INSERT, 0, "a", s.precedence)) 
    s.apply_local_change(jup.Operation(jup.OP_INSERT, 1, "b", s.precedence))

    r1.handle_remote_ack()
    r2.handle_remote_ack()

    print s

class AsyncBuffer:
    def __init__(self):
        self.events = []

    def make_change_sender(self, other_remote):
        def _change_sender(change):
            # print "Change being sent:", change
            def _send_change():
                # print "Change being executed:", change
                other_remote.handle_remote_change(change)
            self.events.append(_send_change)
        return _change_sender

    def make_ack_sender(self, other_remote):
        def _ack_sender():
            # print "Ack being sent"
            def _send_ack():
                other_remote.handle_remote_ack()
            self.events.append(_send_ack)
        return _ack_sender

    def resolve_events(self):
        for event in self.events:
            event()
        events = []

def bad_concurrent_test():
    # make the main server first, and give it some value
    main_server = jup.Server(precedence=3, name="main")
    main_server.apply_local_change(
        jup.Operation(jup.OP_INSERT, 0, "a", main_server.precedence))

    # make the remotes
    ext_to_main_remote = jup.Remote()
    main_to_ext_remote = jup.Remote()

    # use this state as the initialization point
    ext_initer = jup.RemoteInitializer(main_server)
    main_server.add_remote(main_to_ext_remote)

    # make the external server
    ext_server = jup.Server(origin_remote=ext_to_main_remote,
                            remote_init=ext_initer,
                            precedence=2,
                            name="ext")

    # tie the remotes together
    buf = AsyncBuffer()

    ext_to_main_remote.set_local_change_cb(
        buf.make_change_sender(main_to_ext_remote))
    ext_to_main_remote.set_ack_remote_change_cb(
        buf.make_ack_sender(main_to_ext_remote))

    main_to_ext_remote.set_local_change_cb(
        buf.make_change_sender(ext_to_main_remote))
    main_to_ext_remote.set_ack_remote_change_cb(
        buf.make_ack_sender(ext_to_main_remote))

    # send some changes
    ext_server.apply_local_change(
        jup.Operation(jup.OP_INSERT, 1, "d", ext_server.precedence))
    ext_server.apply_local_change(
        jup.Operation(jup.OP_INSERT, 2, "e", ext_server.precedence))

    main_server.apply_local_change(
        jup.Operation(jup.OP_INSERT, 0, "c", main_server.precedence)) 
    main_server.apply_local_change(
        jup.Operation(jup.OP_INSERT, 2, "b", main_server.precedence))

    buf.resolve_events()

    print main_server.value
    print ext_server.value

if __name__ == "__main__":
    concurrent_test()
