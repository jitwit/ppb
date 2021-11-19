NB. probability of adoption

NB. x E y -- expected score between game including player's of
NB. Elo x and y.
E =: 1 % 1 + 10 ^ 400 %~ -~

NB. x ER y -- given strength x and ratings y, the expected results
NB. according to the elo model.
ER =: +/ . E

NB. markov chain matrix representing state of adoption match.
NB. M_ij : 1 if i=j=n
NB.        p if i+1=j
NB.        1-p if j=0, i<n
NB.        0 otherwise
w =: * (=<:)"0/~ @ i.
l =:  -.@[ ,. 0 $~ ],<:@]
a =: 1 ,~ 0 #~ <:
m =: }: @: (l + w) , a@]
M =: m >:

NB. x m adopt y -- probability that x can adopt y in a match of m games.
A =: {{ {: {. A&(+/ . *)^:(m-1) A =. (x ER y) M 10 }}
adopt =: A :: 'bad input'
