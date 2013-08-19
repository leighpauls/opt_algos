import asyncore, socket, json

from ..algo import operation, Initializer, Ack, Change
from ..algo.client import Client

class CortexClient(asyncore.dispatcher):
    def __init__(self, host, port):
        asyncore.dispatcher.__init__(self)
        self._cortex_client = None
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        self.connect((host, port))

    def handle_read(self):
        data = self.recv(8192)
        if not data:
            print "got a read with no data....?"
            return
        obj = json.loads(data)
        obj_type = obj["type"]
        if obj_type == "server_change":
            print "applying change: " + data
            self._cortex_client.apply_server_change(
                Change.from_dict(obj["change"]))
            print "new tree: ", self._cortex_client.value.to_dict()
        elif obj_type == "server_ack":
            self._cortex_client.apply_server_ack(Ack.from_dict(obj["ack"]))
        elif obj_type == "server_initializer":
            print "got init:", obj["initializer"]
            self._cortex_client = Client(
                self._on_client_change,
                Initializer.from_dict(obj["initializer"]))
            self._cortex_client.value.local_op_append_child()
        else:
            print "unknwon message recieved: " + data

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
