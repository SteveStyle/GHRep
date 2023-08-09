def exhaustive_search(n):
    num_digits = len('{0:b}'.format(n))
    count=0
    comb = combinations_with_replacement(['{0:b}'.format(i).zfill(num_digits) for i in range(1, n+1)], 3)
    for heaps in comb:
        heapsums = [0 for i in range(0, num_digits)]
        for heap in heaps:
            for d in range(0, num_digits):
                heapsums[d] += int(heap[d])
        if not True in (heapsum % 2 != 0 for heapsum in heapsums):
            count +=1
    return count

def formula(n):
    num_digits = len('{0:b}'.format(n))
    k = num_digits - 1
    nn = n - 2**k
    return nn*(nn+1)/2 + (2**(2*k-1)+1)/3 - 2**(k-1)
    

from itertools import combinations_with_replacement
for n in range(1, 32):
    
    print(n, exhaustive_search(n), formula(n))

