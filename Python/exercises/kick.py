from math import *

C_D     = 0.4                   #           drag coefficient
Q       = 1.2                   # kg/m^3    air density
g       = 9.81                  # m/s^2     earth surface gravity

a       = 0.11                  # m         football radius
m       = 0.43                  # kg        football mass

A       = pi*a**2               # m^2       football cross-sectional area

Vinput  = 120                   # km/h      football velocity, as input

def calcForces( xV ):
    V   = xV*1000/(60*60)   # m/s       football velocity, SI units
    Fd  = 0.5*C_D*Q*A*V**2      # kg m/s^2  drag force
    Fg  = m*g                   # kg m/s^2  gravitional force
    print (
'''Drag force is %.2f N.
Gravitational force is %.2f N.
Drag force / gravitational force is %.2f''' %( Fd, Fg, Fd/Fg ) )

print( 'Hard kick, 120 km/h.')
calcForces( 120 )

print( '\nSoft kick, 30 km/g.')
calcForces( 30 )


