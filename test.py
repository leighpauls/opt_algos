
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

def make_change_sender(other_remote):
    def _change_sender(change):
        print "Change being sent:", change
        other_remote.handle_remote_change(change)
    return _change_sender

def make_ack_sender(other_remote):
    def _ack_sender():
        print "Ack being sent"
        other_remote.handle_remote_ack()
    return _ack_sender

def unconcurrent_test():
    # make the main server first, and give it some value
    main_server = jup.Server()
    main_server.apply_local_change(
        jup.Operation(jup.OP_INSERT, 0, "a", main_server.precedence))

    # make the remotes
    ext_to_main_remote = jup.Remote()
    main_to_ext_remote = jup.Remote()

    # use this state as the initialization point
    ext_initer = jup.RemoteInitializer(main_server)
    main_server.add_remote(main_to_ext_remote)

    # make the external server
    ext_server = jup.Server(ext_to_main_remote, ext_initer)

    # tie the remotes together
    ext_to_main_remote.set_local_change_cb(make_change_sender(main_to_ext_remote))
    
    ext_to_main_remote.set_ack_remote_change_cb(make_ack_sender(main_to_ext_remote))

    main_to_ext_remote.set_local_change_cb(make_change_sender(ext_to_main_remote))
    main_to_ext_remote.set_ack_remote_change_cb(make_ack_sender(ext_to_main_remote))

    # send some changes
    main_server.apply_local_change(
        jup.Operation(jup.OP_INSERT, 0, "c", main_server.precedence)) 
    main_server.apply_local_change(
        jup.Operation(jup.OP_INSERT, 2, "b", main_server.precedence))

    print main_server.value
    print ext_server.value

if __name__ == "__main__":
    unconcurrent_test()
