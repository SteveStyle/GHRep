from pprint import pprint
import numpy as np

xs = np.array(range(81)).reshape(9,9)

print(type(xs), type(xs[0]), type(xs[0][0]))

print(xs)

xrow    = tuple( l for l in xs )
xcol    = tuple( xs[:,i] for i in range(9))
xblock  = tuple( xs[3*(i%3):3*(i%3)+3, 3*(i//3):3*(i//3)+3].reshape(9) for i in range(9) )

pprint(xcol)

xcol[2][0]=22
pprint(xcol)

xblock[1][3] = 3366

i = 1; xblock1 = xs[3*(i%3):3*(i%3)+3, 3*(i//3):3*(i//3)+3]
xblock1[1,0] = 333666
xblock1_2 = xblock1.reshape(9)
xblock1_2[4] = 3377

print(xs)

print( xblock1, '\n', xblock1_2 )
