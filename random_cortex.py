import random

chars = [chr(x) for x in xrange(ord('a'), ord('z')+1)]

def _get_node_from_client(client, idx):
    cur_node = client.value
    for i in idx:
        cur_node = cur_node.children[i]
    return cur_node

def _do_random_insert(client):
    cur_node = client.value
    
    # find a random node to apply to
    while len(cur_node.children) > 0 and random.choice([True, False]):
        next_idx = random.randint(0, len(cur_node.children)-1)
        cur_node = cur_node.children[next_idx]

    linear_idx = random.randint(0, len(cur_node.value))
    insert_char = random.choice(chars)

    cur_node.local_op_insert_char(linear_idx, insert_char)
    return True

def _do_random_delete(client):
    cur_node = client.value
    
    # find a node to apply to
    while len(cur_node.children) > 0 and random.choice([True, False]):
        next_idx = random.randint(0, len(cur_node.children)-1)
        cur_node = cur_node.children[next_idx]

    if len(cur_node.value) == 0:
        return False

    linear_idx = random.randint(0, len(cur_node.value)-1)
    cur_node.local_op_delete_char(linear_idx)
    return True

def _do_random_create(client):
    cur_node = client.value

    while len(cur_node.children) > 0 and random.choice([True, False]):
        next_idx = random.randint(0, len(cur_node.children)-1)
        cur_node = cur_node.children[next_idx]

    def do_append():
        cur_node.local_op_append_child()
        return True
    def do_prepend():
        cur_node.local_op_prepend_child()
        return True
    def do_create_before():
        if cur_node is client.value:
            return False
        cur_node.local_op_create_before()
        return True
    def do_create_after():
        if cur_node is client.value:
            return False
        cur_node.local_op_create_after()
        return True

    return random.choice([
            do_append,
            do_prepend,
            do_create_before,
            do_create_after
            ])()

def _do_random_remove(client):
    cur_node = client.value
    
    while len(cur_node.children) > 0 and random.choice([True, False]):
        next_idx = random.randint(0, len(cur_node.children)-1)
        cur_node = cur_node.children[next_idx]

    # don't remove the root
    if cur_node is client.value:
        return False

    cur_node.local_op_remove()
    return True

def _do_random_move(client):
    src_node = client.value

    while len(src_node.children) > 0 and random.choice([True, False]):
        next_idx = random.randint(0, len(src_node.children)-1)
        src_node = src_node.children[next_idx]

    if src_node is client.value:
        return False

    dest_node = client.value
    
    while random.choice([True, False]) and len(dest_node.children) > 1:
        next_idx = random.randint(0, len(dest_node.children)-1)
        dest_node = dest_node.children[next_idx]

    # make sure that dest_node is not (a child of) or (equal to) src_node
    backtrack_node = dest_node
    while backtrack_node is not None:
        if backtrack_node is src_node:
            return False
        backtrack_node = backtrack_node.parent
    
    def do_move_after():
        if dest_node is client.value:
            return False
        src_node.local_op_move_after(dest_node)
        return True
    def do_move_before():
        if dest_node is client.value:
            return False
        src_node.local_op_move_before(dest_node)
        return True
    def do_move_append():
        src_node.local_op_move_append(dest_node)
        return True
    def do_move_prepend():
        src_node.local_op_move_prepend(dest_node)
        return True

    return random.choice([
            do_move_after,
            do_move_before,
            do_move_append,
            do_move_prepend
            ])()

_op_choices = [_do_random_insert,
               _do_random_delete,
               _do_random_create,
               _do_random_remove,
               _do_random_move]
def do_random_op(client):
    done = False
    while not done:
        done = random.choice(_op_choices)(client)

