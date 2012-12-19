import algos.wave as wave

def make_printer(prefix):
    def _printer(a):
        print prefix, ":", a
    return _printer

def simple_client_test():
    client = wave.Client([], 0, make_printer("Client Sending"), 1)
    
    client.apply_local_change(wave.Operation.INSERT, 0, "a")

    print client

if __name__ == "__main__":
    simple_client_test()
