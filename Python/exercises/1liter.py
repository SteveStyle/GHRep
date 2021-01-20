def readDensities():
    fDensity = open('D:\GHRep\scipro-primer\src-4th\\files\densities.dat', 'r')
    for line in fDensity:
        substance = line[:12].strip()
        density = float(line[12:].strip())
        dDensity[substance] = density

def calcMass( xDensity, xVolume ):
    return xDensity * xVolume


dDensity = {}
readDensities()

substances = ['iron', 'air', 'gasoline', 'ice', 'human body', 'silver', 'platinum','proton']

for sub in substances:
    dens = dDensity[sub]
    mass = calcMass( dens, 1000 ) ## mass in grams
    if mass > 1e15:
        units = 'etagrams'
        mass = mass/10**15
    else:
        units = 'grams'
    print('The mass of one litre of %s is %f %s.'%(sub,mass,units))
