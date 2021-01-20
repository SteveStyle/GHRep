from math import *

def gaussian( xx, xsd = 1, xmean = 0 ):
    return exp( - ((xx - xmean)/xsd)**2 / 2) / (sqrt(2*pi)*xsd)

print ( gaussian( 1, xsd = 2 ) )
