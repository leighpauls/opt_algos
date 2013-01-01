import algos.cortex as cortex
from cortex import ValueNode, Operation
from cortex.server import Server
from cortex.client import Client

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
        op_type=Operation.VALUE_INSERT,
        tree_index=[],
        value_index=1,
        value="q")
    client_1.apply_local_change(
        op_type=Operation.SUBTREE_INSERT,
        tree_index=[0])
    client_1.apply_local_change(
        op_type=Operation.VALUE_INSERT,
        tree_index=[0],
        value_index=0,
        value="w")

    client_2 = new_test_client(server)

    print client_1
    print client_2
    
if __name__ == "__main__":
    simple_cortex_test()


