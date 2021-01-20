captions = 'FCA'

print('{0[0]:^3} {0[1]:^6s} {0[2]:^6s}'.format(captions))
for f in range(0,101,10):
    c = (f-32)*5/9; ca = (f-30)/2
    print('{:>3d} {:0=+6.2f} {:0=+6.2f}'.format(f,c,ca))
