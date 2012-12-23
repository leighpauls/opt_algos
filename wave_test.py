import algos.wave as wave
import algos.wave.client
import algos.wave.server

def make_printer(prefix):
    def _printer(a):
        print prefix, ":", a
    return _printer

def simple_client_test():
    server = wave.server.Server(['z'])
    client = None
    
    def handle_server_change(change):
        client.apply_server_change(change)
    def handle_ack(ack):
        client.apply_server_ack(ack)

    remote = server.add_new_remote(
        handle_server_change=handle_server_change,
        handle_ack_available=handle_ack)

    def handle_client_change(change):
        remote.client_change_available(change)

    init_object = remote.get_initializer()
    client = wave.client.Client(handle_client_change, init_object)
    
    client.apply_local_change(wave.Operation.INSERT, 0, "a")
    client.apply_local_change(wave.Operation.INSERT, 1, "b")

    print client

if __name__ == "__main__":
    simple_client_test()
    
