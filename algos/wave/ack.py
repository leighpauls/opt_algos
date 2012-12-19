from printable import Printable

class Ack(Printable):
    def __init__(self, client_state, rel_server_state):
        self.client_state = client_state
        self.rel_server_state = rel_server_state
