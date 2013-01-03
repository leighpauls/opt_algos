from algos.cortex import ValueNode, operation
from algos.cortex.server import Server
from algos.cortex.client import Client

def new_test_client(server):
    remote = server.add_new_remote(
        handle_server_change=lambda change: client.apply_server_change(change),
        handle_ack_available=lambda ack: client.apply_server_ack(ack))
    client = Client(
        send_change_cb=lambda change: remote.client_change_available(change),
        init=remote.get_initializer())
    return client


def simple_cortex_test():
    # make some initial values
    child = ValueNode(value=["a","b","c"])
    parent = ValueNode(value=["z","x"], children=[child])
    server = Server(parent)

    client_1 = new_test_client(server)
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

    client_2 = new_test_client(server)

    print server.value
    print client_1._value
    print client_2._value
    
if __name__ == "__main__":
    simple_cortex_test()


