class Ack:
    def __init__(self, client_state, rel_server_state):
        self.client_state = client_state
        self.rel_server_state = rel_server_state

    def to_dict(self):
        return {
            "client_state": self.client_state,
            "rel_server_state": self.rel_server_state}
    @staticmethod
    def from_dict(obj):
        return Ack(
            obj["client_state"],
            obj["rel_server_state"])
