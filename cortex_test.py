from algos.cortex import ValueNode, operation
from algos.cortex.server import Server
from algos.cortex.client import Client
from concur import ConcurrentBuffer

def new_test_client(server, buf):
    def handle_server_change(change):
        buf.push_event(lambda: client.apply_server_change(change))
    def handle_ack(ack):
        buf.push_event(lambda: client.apply_server_ack(ack))
    def handle_client_change(change):
        buf.push_event(lambda: remote.client_change_available(change))
    
    remote = server.add_new_remote(
        handle_server_change=handle_server_change,
        handle_ack_available=handle_ack)
    client = Client(
        send_change_cb=handle_client_change,
        init=remote.get_initializer())
    return client


def simple_cortex_test():
    # make some initial values
    child = ValueNode(value=["a","b","c"])
    parent = ValueNode(value=["z","x"], children=[child])
    server = Server(parent)

    buf = ConcurrentBuffer()
    
    client_1 = new_test_client(server, buf)
    client_1.apply_local_change(
        operation.Insert,
        [],
        1,
        "q")
    client_1.apply_local_change(
        operation.Create,
        [0])
    client_1.apply_local_change(
        operation.Insert,
        [0],
        0,
        "w")

    client_2 = new_test_client(server, buf)

    print server.value
    print client_1._value
    print client_2._value
    
    buf.resolve_events()

    print server.value
    print client_1._value
    print client_2._value

    # client_1.apply_local_change(
    #     operation.Create,
    #     [1])
    # client_2.apply_local_change(
    #     operation.Remove,
    #     [0])
    
    # print server.value
    # print client_1._value
    # print client_2._value

    # buf.resolve_events()
    
if __name__ == "__main__":
    simple_cortex_test()


