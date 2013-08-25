import sys

def err(*message):
    _do_print(sys.stderr, *message)

def info(*message):
    _do_print(sys.stderr, *message)

def _do_print(stream, *msg):
    for m in msg[:-1]:
        print >> stream, m,
    print >> stream, msg[-1]
    
