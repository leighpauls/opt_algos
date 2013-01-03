import algos.wave as wave
import algos.wave.client
import algos.wave.server
from concur import ConcurrentBuffer

import random

def make_client(server, buf):
    def handle_server_change(change):
        buf.push_event(lambda:client.apply_server_change(change))
    def handle_ack(ack):
        buf.push_event(lambda:client.apply_server_ack(ack))
    def handle_client_change(change):
        buf.push_event(lambda:remote.client_change_available(change))

    remote = server.add_new_remote(
        handle_server_change=handle_server_change,
        handle_ack_available=handle_ack)
    client = wave.client.Client(handle_client_change, remote.get_initializer())

    return client

def simple_client_test():
    server = wave.server.Server(['z'])
    buf = ConcurrentBuffer()

    client_1 = make_client(server, buf)
    client_1.apply_local_change(wave.operation.Insert, 0, "a")
    client_1.apply_local_change(wave.operation.Insert, 1, "b")

    buf.resolve_events()

    client_2 = make_client(server, buf)

    print "pre-change\nc2:", client_2.value
    client_2.apply_local_change(wave.operation.Delete, 1)

    client_1.apply_local_change(wave.operation.Insert, 1, "x")

    print "pre-resolution\nc1:", client_1.value
    print "c2:", client_2.value, ""

    buf.resolve_events()

    print "c1:", client_1.value
    print "c2:", client_2.value
    print server

def monte_carlo_test(seed):
    random.seed(seed)

    chars = [ chr(x) for x in xrange(ord('a'), ord('z')+1) ]
    start_len = random.randint(0, 10)
    start_value = [ random.choice(chars) for _ in xrange(start_len)]

    server = wave.server.Server(start_value)
    buf = ConcurrentBuffer()

    num_clients = random.randint(1, 5)
    clients = [ make_client(server, buf) for _ in xrange(num_clients) ]

    num_cycles = random.randint(1, 10)
    for i in xrange(num_cycles):
        for client in clients:
            num_ops = random.randint(0, 10)
            for _ in xrange(num_ops):
                value_len = len(client.value)
                is_insert = (value_len == 0) or random.choice([True, False])
                if is_insert:
                    client.apply_local_change(
                        wave.operation.Insert,
                        random.randint(0, value_len),
                        random.choice(chars))
                else:
                    client.apply_local_change(
                        wave.operation.Delete,
                        random.randint(0, value_len-1))
        num_events = buf.get_num_events()
        if num_events > 0:
            num_to_leave = random.randint(0, num_events - 1)
            buf.resolve_events(num_to_leave)
        # TODO: add some new clients
        # TODO: remove some existing clients

    # resolve the rest of the events
    buf.resolve_events()
    reference_value = clients[0].value
    passed = True
    for client in clients[1:]:
        if client.value != reference_value:
            passed = False
    if not passed:
        raise "Failed test" + seed
    print "passed", seed
    return passed

def main():
    # simple_client_test()
    # monte_carlo_test(0)
    for i in xrange(0, 100000):
        monte_carlo_test(i)

if __name__ == "__main__":
    main()

    
