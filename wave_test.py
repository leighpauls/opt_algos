import algos.wave as wave
import algos.wave.client
import algos.wave.server

def make_printer(prefix):
    def _printer(a):
        print prefix, ":", a
    return _printer

def simple_client_test():
    server = wave.server.Server(['z'])
    
    def handle_server_change(change):
        client.apply_server_change(change)
    def handle_ack(ack):
        client.apply_server_ack(ack)
    def handle_client_change(change):
        remote.client_change_available(change)

    def handle_server_change_2(change):
        client_2.apply_server_change(change)
    def handle_ack_2(ack):
        client_2.apply_server_ack(ack)
    def handle_client_change_2(change):
        remote_2.client_change_available(change)

    remote = server.add_new_remote(
        handle_server_change=handle_server_change,
        handle_ack_available=handle_ack)

    init_object = remote.get_initializer()
    client = wave.client.Client(handle_client_change, init_object)
    
    client.apply_local_change(wave.Operation.INSERT, 0, "a")
    client.apply_local_change(wave.Operation.INSERT, 1, "b")

    remote_2 = server.add_new_remote(
        handle_server_change=handle_server_change_2,
        handle_ack_available=handle_ack_2)


    init_object_2 = remote_2.get_initializer()
    client_2 = wave.client.Client(handle_client_change_2, init_object_2)
    client_2.apply_local_change(wave.Operation.DELETE, 1, None)

    print client
    print client_2
    print server

if __name__ == "__main__":
    simple_client_test()
    
