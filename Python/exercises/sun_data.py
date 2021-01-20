from pprint import *

fSun = open( 'D:\GHRep\scipro-primer\src-4th\misc\Oxford_sun_hours.txt', 'r')

# The following code reads the files and creates a table (nested list) of values,
# with one row per year 1929-2009, and one column per month.
# 
# Read each line from the file.
# Throw away the first and last which are just '[\n' or ']\n', using length to identify them.
# Remove leading and trailing brackets, whitespace and commas.
# The result is a string of numbers seperated by commas.  Split by comma to get a list of strings.
# Map float() to each string in the list to convert to a number.
# Finally convert the resulting map object back to a list.
# Each of these lists is collected into a larger list to create a table of values.

tSun = [list(map(float, l.strip('[],\n ').split(',') ))
        for l in fSun if len(l)>2 
       ] 

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
