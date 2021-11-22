NB. probability of adoption

NB. x E y -- expected score between game including player's of
NB. Elo x and y.
E =: 1 % 1 + 10 ^ 400 %~ -~

NB. markov chain matrix representing state of adoption match.
NB. M_ij :
w =: * (=<:)"0/~ @ i.             NB. p if i+1=j
a =: <:@*: = i.@,~                NB. 1 if i=j=n
l =: -.@[ * 1 (|.!.0) 0=]|i.@,~@] NB. 1-p if j=0, i<n
m =: w + l + a@]                  NB. sum / 0 otherwise
M =: m >:                         NB. m with proper dimensions

NB. x m adopt y -- probability that x can adopt y in a match of m games.
A =: {{ {: {. A&(+/ . *)^:(m-1) A =. (x E y) M 10 }}
adopt =: A :: 'bad input'
