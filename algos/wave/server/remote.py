
class Remote(Printable):
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
                 initializer):
        self._on_new_remote_change = on_new_remote_change
        self._on_new_server_change = on_new_server_change
        self._on_ack_available = on_ack_available
        self._initializer = initializer
    def client_change_available(self, change):
        self._on_new_remote_change(change)
    def server_change_available(self, change):
        self._on_new_server_change(change)
    def server_ack_to_client(self, ack):
        self._on_ack_available(ack)
    def get_initializer(self):
        return self._initializer
