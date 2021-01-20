a= 23
b=1007
n=68

h = (b-a)/n

l1 = []
for i in range(0,n+1): l1.append(a+i*h)

from pprint import *
pprint(l1)

print()
l2=[a+i*h for i in range(0,n+1)]
pprint(l2)
