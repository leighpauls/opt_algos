import algos.wave as wave

def make_printer(prefix):
    def _printer(a):
        print prefix, ":", a
    return _printer

def simple_client_test():
    client = wave.Client([], 0, make_printer("Client Sending"), 1)
    
    client.apply_local_change(wave.Operation.INSERT, 0, "a")
    client.apply_local_change(wave.Operation.INSERT, 1, "b")
    client.apply_server_ack(wave.Ack(1, 0))
    client.apply_server_change(wave.Change(
            src_client_state=1,
            src_rel_server_state=0,
            op=wave.Operation.INSERT, pos=1, val="c",
            precedence=0))

    
    print client

if __name__ == "__main__":
    simple_client_test()
