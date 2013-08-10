from state import State

class Remote:
    """The interface between a Server and a Client
    Attributes:
    _on_new_remote_data -- cb to the Server, (Change) -> None
    _on_new_server_data -- cb to the Client, (Change) -> None
    _on_ack_available -- cb to the Client (Ack) -> None
    _initializer -- Initializer object for the remote Client
    """
    def __init__(self,
                 on_new_remote_change,
                 on_new_server_change,
                 on_ack_available,
                 initializer,
                 initial_state,
                 on_remote_closed):
        self._on_new_remote_change = on_new_remote_change
        self._on_new_server_change = on_new_server_change
        self._on_ack_available = on_ack_available
        self._initializer = initializer
        self.last_acked_state = State(initial_state)
        self._on_remote_closed = on_remote_closed

    def client_change_available(self, change):
        self._on_new_remote_change(change)
    def server_change_available(self, change):
        self._on_new_server_change(change)
    def server_ack_to_client(self, ack, acking_state):
        self.last_acked_state = State(acking_state)
        self._on_ack_available(ack)
    def get_initializer(self):
        return self._initializer

    def close(self):
        self._on_remote_closed()
