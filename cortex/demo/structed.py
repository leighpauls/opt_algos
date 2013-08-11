from ..algos.cortex import client as client, operation as operation 
from operation import Insert, Delete, Create, Remove, Move

import sys

ALL_OPERATIONS = [
    operation.Insert,
    operation.Delete,
    operation.Create,
    operation.Remove,
    operation.Move]

class Structed:
    def __init__(self, cortex_client):
        self._client = cortex_client
        self._bind_handlers_recursive(self._client.value)

    def bind_handlers_recursive(self, value):
        value.add_listener(ALL_OPERATIONS, self._handle_change)
        for child in value.children:
            self.bind_handlers_recursive(child)

    def _handle_change(self, event):
        # HACK: just stream the whole tree back
        sys.stdout.writelines([self._client.value.to_json()])

def main():
    while True:
        cmd_string = sys.stdin.readline()
        

if __name__ == "__main__":
    main()
