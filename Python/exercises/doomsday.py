from time import *

weekdays = ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')

doomsday_dates = ['2020-04-04','2020-06-06','2020-08-08','2020-10-10','2020-10-31','2020-12-12',
             '2020-12-26','2021-01-02','2021-02-06']

dtFormat = '%Y-%m-%d'
dmDay2020 = '2020-02-29'
dmDay2020st = strptime('2020-02-29','%Y-%m-%d')
dmDay2020wday = dmDay2020st.tm_wday
print( strptime(dmDay2020,dtFormat), dmDay2020wday )

def str2wday( iDt ): return (strptime(iDt,dtFormat).tm_wday +1)%7

def doomsday( iDom, iMonth, iYear ):
    assert iDom in range(32)
    assert iMonth in range(1,13)
    year = iYear if iMonth >= 2 else iYear-1

##    yearday = iDom if iMonth = 3
##              else 
    

test_dates = ['2020-04-04','2020-06-06','2020-08-08','2020-10-10','2020-10-31','2020-12-12',
             '2020-12-26','2021-01-02','2021-01-30','2021-02-06']

for dt in test_dates:
    print( '{:s}: {:2d} {:}'.format(dt,str2wday(dt),weekdays[str2wday(dt)] ))


doomsday( 29,4,2020 )
