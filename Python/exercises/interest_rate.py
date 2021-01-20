def compoundInterest( xamount, xinterestpc, xperiods ):
    return xamount * (1 + xinterestpc/100)**xperiods

A = 1000; i = 5; periods = 3
final = compoundInterest( A, i, periods)
print("After %d years at %d%% interest, %d euros will be worth %.2f euros"%(periods, i, A, final))
