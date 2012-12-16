from containers import *

class Remote(Printable):
    """Represents a remote server on the local machine to the server.
    
    Attributes:
    local_change_cb -- function to send changes to the remote with, accepts a Change object
    ack_remote_change_cb -- funciton to call when a change from the remote has been applied
    local_change_queue -- holds changes until an ack has been recieved for the last changes
    pending_acks -- number of ACKS I need before sending another change
    remote_change_handler -- call this when the remote has something to apply to the local server
    remote_change_queue -- where to hold the remote changes until the server is ready to take them
    """

    # configuration methods
    def __init__(self):
        self.local_change_cb = None
        self.ack_remote_change_cb = None
        self.local_change_queue = []
        self.pending_acks = 0
        self.remote_change_handler = None
        self.remote_change_queue = []

    def set_local_change_cb(self, cb):
        self.local_change_cb = cb
        self._try_send_queue()

    def set_ack_remote_change_cb(self, cb):
        self.ack_remote_change_cb = cb;

    # inbound handlers from the remote server
    def handle_remote_change(self, remote_change):
        """Called externally to push changes into the local server.
        Attributes:
        remote_change -- a Change object
        """
        if not self.remote_change_handler == None:
            self.remote_change_handler(remote_change)
        else:
            self.remote_change_queue.append(remote_change)

    def handle_remote_ack(self):
        """Called externally to indicate that pushed changes have been accepted"""
        if self.pending_acks == 0:
            raise "Tried to ack when no change had been sent"
        self.pending_acks -= 1
        self._try_send_queue()

    # control methods for use from the local Server
    def send_remote_change_ack(self):
        """Called by the Server to indicate that the last change was applied."""
        if not self.ack_remote_change_cb:
            raise "Changed was applied and acked before ack_remote_change_cb was set"
        self.ack_remote_change_cb()


    def set_remote_change_handler(self, cb):
        """Called by Server to indicate how to indicate external changes.
        Attributes:
        cb -- to be called for external changes, acceps a Change
        """
        self.remote_change_handler = cb
        if not self.remote_change_queue == None:
            for remote_change in self.remote_change_queue:
                self.remote_change_handler(remote_change)
        remote_change_queue = None
        
    def enqueue_local_change(self, change):
        """Called by Server when there's a local change to send.
        Attribtues:
        change - a new Change object
        """
        self.local_change_queue.append(change)
        self._try_send_queue()

    def _try_send_queue(self):
        if self.pending_acks is not 0 or \
                len(self.local_change_queue) == 0 or \
                not self.local_change_cb:
            return
        self.pending_acks += 1
        self.local_change_cb(self.local_change_queue.pop(0))


