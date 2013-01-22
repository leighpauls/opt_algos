import random
from algos.cortex.operation import Insert, Delete, Create, Remove, Move

chars = [ chr(x) for x in xrange(ord('a'), ord('z')+1) ]

def _do_random_insert(client):
    node_idx = []
    cur_node = client.value
    
    # find a random node to apply to
    while len(cur_node.children) > 0 and random.choice([True, False]):
        next_idx = random.randint(0, len(cur_node.children)-1)
        node_idx.append(next_idx)
        cur_node = cur_node.children[next_idx]

    linear_idx = random.randint(0, len(cur_node.value))
    insert_char = random.choice(chars)

    client.apply_local_change(Insert, node_idx, linear_idx, insert_char)
    return True

def _do_random_delete(client):
    node_idx = []
    cur_node = client.value
    
    # find a node to apply to
    while len(cur_node.children) > 0 and random.choice([True, False]):
        next_idx = random.randint(0, len(cur_node.children)-1)
        node_idx.append(next_idx)
        cur_node = cur_node.children[next_idx]

    if len(cur_node.value) == 0:
        return False

    linear_idx = random.randint(0, len(cur_node.value)-1)
    client.apply_local_change(Delete, node_idx, linear_idx)
    return True

def _do_random_create(client):
    node_idx = []
    cur_node = client.value

    while len(cur_node.children) > 0 and random.choice([True, False]):
        next_idx = random.randint(0, len(cur_node.children)-1)
        node_idx.append(next_idx)
        cur_node = cur_node.children[next_idx]

    node_idx.append(random.randint(0, len(cur_node.children)))
    client.apply_local_change(Create, node_idx)
    return True

def _do_random_remove(client):
    node_idx = []
    cur_node = client.value
    
    while len(cur_node.children) > 0 and random.choice([True, False]):
        next_idx = random.randint(0, len(cur_node.children)-1)
        node_idx.append(next_idx)
        cur_node = cur_node.children[next_idx]

    if node_idx == []:
        return False

    client.apply_local_change(Remove, node_idx)
    return True

def _do_random_move(client):
    src_node_idx = []
    cur_node = client.value

    while len(cur_node.children) > 0 and random.choice([True, False]):
        next_idx = random.randint(0, len(cur_node.children)-1)
        src_node_idx.append(next_idx)
        cur_node = cur_node.children[next_idx]

    if src_node_idx == []:
        return False

    dest_node_idx = []
    cur_node = client.value
    
    while random.choice([True, False]) \
            and (len(cur_node.children) > 1
                 or (len(cur_node.children) > 0
                     and dest_node_idx != src_node_idx[:-1])):
        if dest_node_idx == src_node_idx[:-1]:
            next_idx = random.randint(0, len(cur_node.children)-2)
            dest_node_idx.append(next_idx)
            if next_idx >= src_node_idx[-1]:
                cur_node = cur_node.children[next_idx+1]
            else:
                cur_node = cur_node.children[next_idx]
        else:
            next_idx = random.randint(0, len(cur_node.children)-1)
            dest_node_idx.append(next_idx)
            cur_node = cur_node.children[next_idx]

    if dest_node_idx == src_node_idx[:-1]:
        dest_node_idx.append(random.randint(0, len(cur_node.children)-1))
    else:
        dest_node_idx.append(random.randint(0, len(cur_node.children)))

    client.apply_local_change(Move, src_node_idx, dest_node_idx)
    return True

_op_choices = [_do_random_insert,
               _do_random_delete,
               _do_random_create,
               _do_random_remove,
               _do_random_move]
def do_random_op(client):
    done = False
    while not done:
        done = random.choice(_op_choices)(client)

