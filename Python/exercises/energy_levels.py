m_e     =   9.1094E-31
e       =   1.6022E-19
eps0    =   8.8542E-12
h       =   6.6261E-34

H       =   m_e*e**4/(8*eps0*h**2)

def E_n(xn): return H/xn**2
    

print('{:>2} {:^12}\n-- ------------'.format('n', 'E_n(n)'))
for n in range(1,21): print('{:2d} {:E}'.format(n, E_n(n)))

print('')

lvls = [[E_n(i)-E_n(f) for i in range(1,6)] for f in range(1,6)]

print('     i=    ',*['{:<14d}'.format(i) for i in range(1,6)],sep='')
print('f=')
for f,r in enumerate(lvls):
    print('{:^3d}'.format(f+1),end='  ')
    for c in r:
        print('{:13E}'.format(c),end=' ')
    print()
