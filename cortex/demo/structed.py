import sys, asyncore, json

from ..algo import client
from ..algo.operation import Insert, Delete, Create, Remove, Move
from ..net import CortexClient
from .. import logging as clog

from operator import Operator

ALL_OPERATIONS = [Insert, Delete, Create, Remove, Move]

"""
Hacky implementation, this should not be multi-process like it is....

Structed operations:
- insert
- delete
- append
- remove
- move
TODO: make these two timed on/off from emacs like typing-debouncing
- hold_local_lock (HACK: does not auto-sync by default for now)
- release_local_lock
"""

class Structed(asyncore.file_dispatcher):
    def __init__(self, net_client):
        asyncore.file_dispatcher.__init__(self, sys.stdin)
        self._net_client = net_client
        self._operator = None

    def handle_read(self):
        """Overrides asyncore.file_dispatcher"""
        data = self.recv(8192)
        try:
            obj = json.loads(data)
        except:
            obj = None

        if obj is None:
            clog.err("invalid read: ", data)
            self._output_tree()
            return

        obj_type = obj["type"]
        if obj_type == "hold_local_lock":
            self._on_local_lock()
        elif obj_type == "release_local_lock":
            self._on_release_lock()
        else:
            self._operator.handle_operation(obj)
            
    def _on_local_lock(self):
        self._net_client.hold_local_lock()
    def _on_release_lock(self):
        self._net_client.release_local_lock()
    
        
    def _bind_handlers_recursive(self, value):
        """ for the initial binding of handlers to the tree"""
        value.add_listener(ALL_OPERATIONS, self._handle_change)
        for child in value.children:
            self._bind_handlers_recursive(child)

    def _handle_change(self, event):
        # HACK: just stream the whole tree back
        self._output_tree()

        # add listeners to any new nodes
        if event.OP_NAME == Create.OP_NAME:
            event.new_node.add_listener(ALL_OPERATIONS, self._handle_change)

    def on_inited(self):
        self._cortex_client = self._net_client.cortex_client
        self._operator = Operator(self._cortex_client.value)
        self._bind_handlers_recursive(self._cortex_client.value)
        self._output_tree()

    def _output_tree(self):
        sys.stdout.write(json.dumps(self._cortex_client.value.to_dict()) + '\n')


def main():
    net_client = CortexClient("localhost", 11111)
    structed = Structed(net_client)
    net_client.register_initialized_callback(structed.on_inited)
    asyncore.loop()

if __name__ == "__main__":
    main()
