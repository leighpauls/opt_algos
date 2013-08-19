import asyncore, socket, json

from ..algo.server import Server
from ..algo import Change

class CortexServerHandler(asyncore.dispatcher_with_send):

    def __init__(self, sock, cortex_server):
        asyncore.dispatcher_with_send.__init__(self, sock)
        self._remote = cortex_server.add_new_remote(
            self._handle_server_change,
            self._handle_ack_available)
        self._send_initializer(self._remote.get_initializer())

    def handle_read(self):
        print "Incomming read!"
        data = self.recv(8192)
        if not data:
            print "got a read with no data...?"
            return
        obj = json.loads(data)
        if obj["type"] == "client_change":
            self._remote.client_change_available(
                Change.from_dict(obj["change"]))
        else:
            print "unknown message recieved: " + data
        
    def handle_close(self):
        print "Socket closed!"
        # TODO: don't close the remote on a physical disconnection (it avoids 
        # the whole point of OT)
        self._remote.close()
        self.close()

    def _send_initializer(self, initializer):
        init_dict = {
            "type": "server_initializer",
            "initializer": initializer.to_dict()}
        self.send(json.dumps(init_dict) + "\n")

    def _handle_server_change(self, change):
        change_dict = {
            "type": "server_change",
            "change": change.to_dict()}
        self.send(json.dumps(change_dict) + "\n")

    def _handle_ack_available(self, ack):
        ack_dict = {
            "type": "server_ack",
            "ack": ack.to_dict()}
        self.send(json.dumps(ack_dict) + "\n")


class CortexServer(asyncore.dispatcher):

    def __init__(self, host, port, cortex_server):
        asyncore.dispatcher.__init__(self)
        self.cortex_server = cortex_server
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        self.set_reuse_addr()
        self.bind((host, port))
        self.listen(5)

    def handle_accept(self):
        pair = self.accept()
        if pair is not None:
            sock, addr = pair
            print 'Incoming connection from %s' % repr(addr)
            handler = CortexServerHandler(sock, self.cortex_server)

def main():
    server = CortexServer('localhost', 11111, Server())
    asyncore.loop()

if __name__ == "__main__":
    main()
