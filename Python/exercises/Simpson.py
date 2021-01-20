def SIntegrate( f, a, b, n ):

    assert n%2 == 0
    
    h = (b-a)/n
    fx = lambda r: f(a +r*h)

    return h/3* (
             f(a)+f(b)+
             4*sum(fx(2*i-1) for i in range(1,n//2+1))+
             2*sum(fx(2*i)   for i in range(1,n//2))
           )

print( SIntegrate( lambda x: x**2, 0, 20, 10) )

from math import sin, pi
print( SIntegrate( lambda x: 3/2*sin(x)**3, 0, pi, 500) )

h = lambda x: 3/2*sin(x)**3

def application():
    print('Integrate 1.5 sin^3 x from 0 to pi.')
    for n in 2,6,12,100,500:
        approx = SIntegrate( h, 0, pi, n)
        print( 'n={:3d}, approx={:18.15f}, error={:9.2E}'.format(n,approx,2-approx) )

application()

print( '\n 3x^2 -7x + 2.5 from 3/2 to 2\n',
       SIntegrate( lambda x: 3*x**2 -7*x +2.5, 3/2, 2, 10) )
