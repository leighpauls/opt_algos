from containers import *

class Remote(Printable):
    def __init__(self, local_change_cb, ack_remote_change_cb, init_remote_cb):
        self.local_change_cb = local_change_cb
        self.ack_remote_change_cb = ack_remote_change_cb
        self.init_remote_cb = init_remote_cb
        self.local_change_queue = []
        self.wait_for_ack = False
        self.remote_change_handler = None
        self.remote_change_queue = []

    def handle_remote_change(self, remote_change):
        if not self.remote_change_handler == None:
            self.remote_change_handler(remote_change)
        else:
            self.remote_change_queue.append(remote_change)

    def set_remote_change_handler(self, cb):
        self.remote_change_handler = cb
        if not self.remote_change_queue == None:
            for remote_change in self.remote_change_queue:
                self.remote_change_handler(remote_change)
        remote_change_queue = None
        
    def try_send_queue(self):
        if self.wait_for_ack or len(self.local_change_queue) == 0:
            return
        self.local_change_cb(self.local_change_queue)
        self.local_change_queue = []
        self.wait_for_ack = True

    def enqueue_local_change(self, change):
        self.local_change_queue.append(change)
        self.try_send_queue()

    def handle_remote_ack(self):
        self.wait_for_ack = False
        self.try_send_queue()
