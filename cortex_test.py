from algos.cortex import ValueNode, operation
from algos.cortex.server import Server
from algos.cortex.client import Client
from concur import ConcurrentBuffer

import random

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
    print
    
    buf.resolve_events()

    print server.value
    print client_1._value
    print client_2._value
    print

    client_1.apply_local_change(
        operation.Create,
        [1])
    client_2.apply_local_change(
        operation.Remove,
        [0])
    
    print server.value
    print client_1._value
    print client_2._value
    print

    buf.resolve_events()

    print server.value
    print client_1._value
    print client_2._value

import random_cortex as rc

def monte_carlo_test(seed):
    random.seed(seed)

    server = Server()
    buf = ConcurrentBuffer()
    
    num_clients = random.randint(1, 5)
    clients = [ new_test_client(server, buf) for _ in xrange(num_clients) ]

    num_cycles = random.randint(1, 10)
    for i in xrange(num_cycles):
        for client in clients:
            num_ops = random.randint(0, 10)
            for _ in xrange(num_ops):
                rc.do_random_op(client)
        buf.resolve_events()

    reference_val = clients[0].value
    for client in clients[1:]:
        if not client.value.is_equal(reference_val):
            raise Exception("Client did not converge, seed: " + str(seed))
    print seed, "worked"

from algos.cortex.operation import Create, Remove, Move

def my_test():
    """Place to put individual probing tests"""
    server_op = Move(None, 5, [2], [0, 0])
    local_op = Move(None, 4, [2], [3])
    server_op.transform(local_op, None)

def main():
    # my_test()
    monte_carlo_test(0)
    # for i in xrange(0, 100):
    #     monte_carlo_test(i)

import sys
if __name__ == "__main__":
    # sys.setrecursionlimit(100)
    main()

