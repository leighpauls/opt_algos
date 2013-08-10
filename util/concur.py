from collections import deque

class ConcurrentBuffer:
    """A buffer for keeping functions from being called to simulate concurrency due to latency"""
    def __init__(self):
        self.events = deque()

    def push_event(self, func):
        self.events.append(func)
    
    def resolve_events(self, num_to_leave=None):
        if num_to_leave is None:
            num_to_leave = 0

        while len(self.events) > num_to_leave:
            self.events.popleft()()

    def get_num_events(self):
        return len(self.events)
