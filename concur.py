from collections import deque

class ConcurrentBuffer:
    """A buffer for keeping functions from being called to simulate concurrency due to latency"""
    def __init__(self):
        self.events = deque()

    def push_event(self, func):
        self.events.append(func)
    
    def resolve_events(self):
        while len(self.events) > 0:
            self.events.popleft()()
