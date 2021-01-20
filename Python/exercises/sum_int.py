def sumint( xn ):
    i=0;a=0
    while i<=xn: a+=i;i+=1
    return a

def sumint2( xn ):
    a=0
    for i in range(1,xn+1): a+=i
    return a

def sumint3( xn ):
    if xn: return xn+sumint3(xn-1)
    else: return 0
    


n=123
print(sumint(n))
print(sumint2(n))
print(sumint3(n))
print(n*(n+1)//2)
