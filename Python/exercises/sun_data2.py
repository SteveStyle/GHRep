from pprint import pprint
import numpy as np

with open( r'D:\GHRep\scipro-primer\src-4th\misc\Oxford_sun_hours.txt', 'r') as fSun:
    tSun = np.array(eval(fSun.read()));

print( len(tSun), type(tSun), type(tSun[0]), type(tSun[0][0]), tSun[0][0], '\n', tSun[0] );

##fSun = open( 'D:\GHRep\scipro-primer\src-4th\misc\Oxford_sun_hours.txt', 'r')
##
##tSun = [[float(v) for v in l.strip('[],\n ').split(',')]
##        for l in fSun if len(l)>2 
##       ] 

lAvg = [sum(t)/len(t) for t in zip(*tSun)]

month_names = 'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'

maxSunshine = max(lAvg); maxMonth = lAvg.index(maxSunshine)

for n,v in zip(month_names,lAvg):
    print( '%3s: %5.1f'%(n,v) )
    
print ('\nThe sunniest month is %s with %.1f hours of sunshine.\n'%(month_names[maxMonth], maxSunshine))

decAvg = [0]*(len(tSun)//10)
for i,row in enumerate(tSun):
    if i>0: decAvg[(i-1)//10] += row[0]
    if i<len(tSun)-1: decAvg[i//10] += row[11]


for i,v in enumerate(decAvg):
    print( 'Decade %d-%d: %4.1f'%(10*i+1930,10*i+1939,v/(10*2*30)) )
