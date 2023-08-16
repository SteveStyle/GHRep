def search_method(n) -> int:
    num_digits = len("{0:b}".format(n))
    count = 0
    comb = combinations_with_replacement(
        ["{0:b}".format(i).zfill(num_digits) for i in range(1, n + 1)], 3
    )
    for heaps in comb:
        heapsums = [0 for i in range(0, num_digits)]
        for heap in heaps:
            for d in range(0, num_digits):
                heapsums[d] += int(heap[d])
        if not True in (heapsum % 2 != 0 for heapsum in heapsums):
            count += 1
    return count


def search_method2(n) -> int:
    count = 0
    comb = combinations_with_replacement(range(1, n + 1), 3)
    for heaps in comb:
        if heaps[0] ^ heaps[1] ^ heaps[2] == 0:
            count += 1
    return count


def formula_method(n):
    num_digits = len("{0:b}".format(n))
    k = num_digits - 1
    m = n - 2**k
    return m * (m + 1) // 2 + (2 ** (2 * k - 1) + 1) // 3 - 2 ** (k - 1)


def formula_method2(n) -> int:
    num_digits = len("{0:b}".format(n))
    k = num_digits - 1
    m = n - 2**k
    return m * (m + 1) // 2 + ((2**k - 1) * (2 ** (k - 1) - 1)) // 3


from itertools import combinations_with_replacement
import time

t1 = time.time()
l = []
for n in range(1, 100):
    #    print(n, search_method(n), search_method2(n), formula_method(n), formula_method2(n))
    #    print(n, search_method2(n), formula_method2(n))
    # print(n, formula_method2(n))
    l.append(formula_method2(n))
print(l)
t2 = time.time()
print(t2 - t1)
