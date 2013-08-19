import asyncore, socket, json

from ..algo import operation, Initializer, Ack, Change
from ..algo.client import Client
from ...util import ConcurrentBuffer

class CortexClient(asyncore.dispatcher):
    def __init__(self, host, port):
        asyncore.dispatcher.__init__(self)
        self._initialized_callbacks = []
        self._cortex_client = None
        self._operation_buffer = ConcurrentBuffer()
        self._local_lock = False
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        self.connect((host, port))

    @property
    def cortex_client(self):
        return self._cortex_client

    def register_initialized_callback(self, cb):
        if self._cortex_client is not None:
            cb()
        else:
            self._initialized_callbacks.append(cb)

    def _alert_new_operations(self):
        if self._local_lock:
            return
        self._operation_buffer.resolve_events()

    def handle_read(self):
        """Overrides asyncore.dispatcher"""
        data = self.recv(8192)
        if not data:
            print "got a read with no data....?"
            return
        obj = json.loads(data)
        obj_type = obj["type"]
        if obj_type == "server_change":
            self._operation_buffer.push_event(
                lambda: self._on_server_change(obj))
            self._alert_new_operations()

        elif obj_type == "server_ack":
            self._operation_buffer.push_event(
                lambda: self._on_server_ack(obj))
            self._alert_new_operations()

        elif obj_type == "server_initializer":
            self._on_server_initializer(obj)
        else:
            print "unknwon message recieved: " + data

    def _on_server_initializer(self, obj):
        print "got init:", obj["initializer"]
        self._cortex_client = Client(
            self._on_client_change,
            Initializer.from_dict(obj["initializer"]))
        # tell the rest of the app that cortex is ready to go
        for cb in self._initialized_callbacks:
            cb()
        self._initialized_callbacks = None

    def _on_server_change(self, obj):
        print "applying change: " + data
        self._cortex_client.apply_server_change(
            Change.from_dict(obj["change"]))
        print "new tree: ", self._cortex_client.value.to_dict()

    def _on_server_ack(self, obj):
        self._cortex_client.apply_server_ack(Ack.from_dict(obj["ack"]))

    def _on_client_change(self, change):
        obj = {
            "type": "client_change",
            "change": change.to_dict()}
        self.send(json.dumps(obj) + "\n")


def main():
    client = CortexClient("localhost", 11111)
    asyncore.loop()

if __name__ == "__main__":
    main()
