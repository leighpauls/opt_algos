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
            ops_applied = []
            for _ in xrange(num_ops):
                ops_applied.append(do_random_operation(client))
        buf.resolve_events()

    reference_val = clients[0]
    for client in clients[1:]:
        if not client == reference_val:
            raise Exception("Client did not converge, seed: " + str(seed))

from algos.cortex.operation import Insert, Delete, Create, Remove, Move

chars = [ chr(x) for x in xrange(ord('a'), ord('z')+1) ]
operations = [Insert, Delete, Create, Remove, Move]
def do_random_operation(client):
    class InvalidIndex(Exception):pass
    
    root_node = client._value
    def get_random_tree_idx(isCreation, isRemoval=False):
        cur_idx = []
        cur_node = root_node
        while len(cur_node.children) > 0 and random.choice([True, False]):
            cur_idx.append(random.randint(0, len(cur_node.children)-1))
            assert(cur_idx[-1] >= 0)
            cur_node = cur_node.children[cur_idx[-1]]

        if isCreation:
            cur_idx.append(random.randint(0, len(cur_node.children)))

        if isRemoval and cur_idx == []:
            raise InvalidIndex()
        assert(len(cur_idx) == 0 or cur_idx[-1] >= 0)
        return cur_idx

    def get_random_linear_idx(tree_idx, isInsertion):
        cur_node = root_node
        for i in tree_idx:
            cur_node = cur_node.children[i]

        if isInsertion:
            return random.randint(0, len(cur_node.value))
        else:
            if len(cur_node.value) == 0:
                raise InvalidIndex()
            return random.randint(0, len(cur_node.value)-1)
        
    completed = False
    while not completed:
        completed = True

        op_class = random.choice(operations)
        if op_class is Insert:
            tree_idx = get_random_tree_idx(False)
            linear_idx = get_random_linear_idx(tree_idx, True)
            val = random.choice(chars)
            client.apply_local_change(Insert, tree_idx, linear_idx, val)

        elif op_class is Delete:
            tree_idx = get_random_tree_idx(False)
            try:
                linear_idx = get_random_linear_idx(tree_idx, False)
            except InvalidIndex:
                completed = False
                continue
            client.apply_local_change(Delete, tree_idx, linear_idx)

        elif op_class is Create:
            tree_idx = get_random_tree_idx(True)
            client.apply_local_change(Create, tree_idx)

        elif op_class is Remove:
            try:
                tree_idx = get_random_tree_idx(False, True)
            except InvalidIndex:
                completed = False
                continue
            client.apply_local_change(Remove, tree_idx)

        elif op_class is Move:
            try:
                src_idx = get_random_tree_idx(False, True)
            except InvalidIndex:
                completed = False
                continue
            dest_idx = get_random_tree_idx(True)
            if len(src_idx) <= len(dest_idx) and src_idx == dest_idx[:len(src_idx)]:
                completed = False
                continue
            client.apply_local_change(Move, src_idx, dest_idx)

        else:
            raise Exception("WTF: " + op_class.__name__)
    
    return op_class

def main():
    # simple_cortex_test()
    monte_carlo_test(0)
    # for i in xrange(0, 100):
    #    monte_carlo_test(i)

import sys
if __name__ == "__main__":
    # sys.setrecursionlimit(100)
    main()

