import pickle

# I would implement this in pickle if itertools was less awful
def walk_predecessor(pred, start, end):
    start -= 1
    end -= 1
    ret = []
    cur = start
    while cur != -1 and cur != pred[cur, end]:
        cur = pred[cur, end]
        ret.append(cur)
    ret.append(end)
    return ret

# pickle utils
def p_int(x):
    return pickle.INT + str(x).encode() + b"\n"

def p_str(x):
    return pickle.BINUNICODE + len(x).to_bytes(4, "little") + x.encode()

def p_imp(mod, prop):
    return pickle.GLOBAL + str(mod).encode() + b"\n" + str(prop).encode() + b"\n"

def p_memo(x):
    return pickle.GET + str(x).encode() + b"\n"

def p_setattrs(vals):
    return pickle.EMPTY_DICT + pickle.MARK + b"".join(p_str(k) + v for (k, v) in vals.items()) + pickle.DICT + pickle.TUPLE2 + pickle.BUILD

def p_setattr(prop, val):
    return p_setattrs({prop: val})

def p_getattr(val, prop):
    return p_imp("pickle", "io") + p_setattr("whatagarbage", val) + pickle.POP + p_imp("io", "whatagarbage." + prop)

header = pickle.PROTO + bytes([4])
footer = b""

footer += pickle.TUPLE2
# memoize the input data
footer += pickle.MEMOIZE + pickle.POP # memo 0
footer += pickle.MEMOIZE + pickle.POP # memo 1
# while we're here prepare the function calls
footer += p_imp("builtins", "print")
footer += p_imp("builtins", "int")
footer += p_imp("operator", "getitem")
footer += p_imp("builtins", "print")
footer += p_imp("itertools", "pairwise")
footer += p_imp("operator", "getitem")
footer += p_imp("python_tsp.exact", "solve_tsp_dynamic_programming")
footer += p_imp("numpy", "pad")
footer += p_imp("operator", "getitem")
footer += p_imp("scipy.sparse.csgraph", "floyd_warshall")
footer += p_imp("scipy.sparse", "csr_matrix")
# make empty sparse matrix
footer += p_imp("scipy.sparse", "dok_matrix") + p_memo(0) + pickle.TUPLE1 + pickle.REDUCE
footer += pickle.MEMOIZE # memo 2
# update sparse matrix with edges
footer += p_getattr(p_memo(2), "_update") + p_memo(1) + pickle.TUPLE1 + pickle.REDUCE
footer += pickle.POP
# do floyd warshall
footer += pickle.TUPLE1 + pickle.REDUCE
footer += pickle.TRUE + pickle.TRUE + pickle.TUPLE3 + pickle.REDUCE
footer += pickle.MEMOIZE # memo 3
footer += p_int(0) + pickle.TUPLE2 + pickle.REDUCE
# add zero node
footer += p_int(1) + p_int(0) + pickle.TUPLE2 + pickle.TUPLE2 + pickle.REDUCE
# do tsp
footer += pickle.TUPLE1 + pickle.REDUCE
footer += pickle.MEMOIZE # memo 4
# process path
footer += p_int(0) + pickle.TUPLE2 + pickle.REDUCE
# walk predecessors
footer += pickle.TUPLE1 + pickle.REDUCE
footer += pickle.MEMOIZE + pickle.POP # memo 5
footer += p_imp("itertools", "chain.from_iterable")
footer += p_imp("itertools", "starmap")
footer += p_imp("functools", "partial")
footer += p_imp(__name__, "walk_predecessor")
footer += p_imp("operator", "getitem")
footer += p_memo(3) + p_int(1) + pickle.TUPLE2 + pickle.REDUCE
footer += pickle.TUPLE2 + pickle.REDUCE
footer += p_memo(5) + pickle.TUPLE2 + pickle.REDUCE
footer += pickle.TUPLE1 + pickle.REDUCE
footer += pickle.MEMOIZE + pickle.POP # memo 6

# load alphabet
footer += p_imp("builtins", "str.join")
footer += p_str(", ")
footer += p_imp("builtins", "map")
# convert list to alphabet
footer += p_getattr(p_imp("string", "ascii_lowercase"), "__getitem__") + p_memo(6) + pickle.TUPLE2 + pickle.REDUCE
footer += pickle.TUPLE2 + pickle.REDUCE
# print path
footer += pickle.TUPLE1 + pickle.REDUCE
footer += pickle.POP
# print distance as an int
footer += p_memo(4) + p_int(1) + pickle.TUPLE2 + pickle.REDUCE
footer += pickle.TUPLE1 + pickle.REDUCE
footer += pickle.TUPLE1 + pickle.REDUCE

footer += pickle.STOP


def solve_graph(graph):
    # serialize graph into pickle
    data = pickle.MARK
    for (i, r) in enumerate(graph):
        for (j, v) in enumerate(r):
            if v != -1:
                data += p_int(i) + p_int(j) + pickle.TUPLE2 + p_int(v)
    data += pickle.DICT
    data += p_int(len(graph))
    data += p_int(len(graph[0]))
    pickle.loads(header + data + footer)

if __name__ == "__main__":
    graph = [
        [0, 20, 42, 35],
        [20, 0, 30, -1],
        [42, 30, 0, 12],
        [35, -1, 12, 0]
    ]
    # graph = [[0, 8, 10, 5, 8, 6, 7, 7, 6, 9], [3, 0, 0, 1, 4, -1, 10, 7, 4, 8], [2, 2, 0, -1, 7, 7, 3, 8, 3, 7], [2, -1, 4, 0, 0, -1, 10, 5, 10, 8], [7, 6, 5, 2, 0, 7, 4, 3, 3, 0], [7, 2, 7, 4, 1, 0, 4, 2, 9, 1], [-1, 3, 1, 0, 10, 1, 0, 0, 9, 5], [6, 2, 10, 8, 1, 0, 1, 0, 1, 3], [8, 8, 3, 4, 10, -1, 8, 3, 0, 8], [-1, 4, 8, 10, 1, 4, 6, 0, 4, 0]]
    solve_graph(graph)
