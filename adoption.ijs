load '../J-esquisse/elo.ijs'
NB. probability of adoption
NB. 10 x # of matches matrix

NB. markov chain matrix representing state of adoption match.
w =: * (=<:)"0/~ @ i.
l =:  -.@[ ,. 0 $~ ],<:@]
a =: 1 ,~ 0 #~ <:
m =: }: @: (l + w) , a@]
M =: m >:
NB. M_ij = matrix of transition probailities
NB. 

NB. x m adopt y -- probability that x can adopt y in a match of m games.
adopt =: {{ {: {. A&(+/ . *)^:(m-1) A =. (x ER_elo_ y) M 10 }}
