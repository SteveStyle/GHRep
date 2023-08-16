def search_method(n) -> int:
    count = 0
    comb = combinations_with_replacement(range(1, n + 1), 3)
    for heaps in comb:
        if (sum(heaps) <= n) and (heaps[0] ^ heaps[1] ^ heaps[2] == 0):
            count += 1
    return count


def formula_method(n):
    hamming_weight = bin(n).count("1")
    n4 = int("{0:b}".format(n), 4)
    return (n4 - 3 * n + 2 * hamming_weight) // 3


from itertools import combinations_with_replacement
import time

t1 = time.time()
l = []
for n in range(1, 51):
    print(n, search_method(n), formula_method(n))
    # print(n, formula_method(n))
    # l.append(formula_method(n))

# print(l)


t2 = time.time()
print(t2 - t1)
