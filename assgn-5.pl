:- op(600, xfy, ->).
:- op(500, xfy, &).

sentence(Rep, S0, S2) :- np(X^Rep1^Rep, S0, S1), vp(X^Rep1, S1, S2).
np(X^Rep1^Rep, S0, S2) :- det(X^Rep2^Rep1^Rep, S0, S1), adj_noun(X^Rep2, S1, S2).
np(X^Rep1^Rep, S0, S3) :- det(X^Rep2^Rep1^Rep, S0, S1), adj_noun(X^Rep3, S1, S2), rel_clause(X^Rep3^Rep2, S2, S3).
np(X^Rep1^Rep, S0, S2) :- proper_noun(X, S0, S1), rel_clause(X^Rep3^Rep2, S2, S3).
np(X^Rep1^Rep, S0, S1) :- proper_noun(X, S0, S1).
vp(X^Rep1, S0, S2) :- t_verb(Y^X^Rep2, S0, S1), np(Y^Rep2^Rep1, S1, S2).
vp(X^Rep1, S0, S2) :- t_verb(Y^X^Rep2, S0, S1), np(Y, S1, S2).
vp(X^Rep1, S0, S2) :- aux(be, S0, S1), comp(X^Rep2^Rep1, S1, S2).
vp(X^Rep1, S0, S1) :- rel_clause(X^Rep1, S0, S1).
vp(Sem, S0, S1) :- in_tverb(Sem, S0, S1).
rel_clause(X^Rep1^and(Rep1, Rep2), S0, S3) :- rel_pron(S0, S1), np(Y^Rep3^Rep2, S1, S2), t_verb(Y^X^Rep3, S2, S3),!.
rel_clause(X^Rep1^and(Rep1, Rep2), S0, S3) :- rel_pron(S0, S1), aux(be, S1, S2), comp(X^Rep2^Rep2, S2, S3),!.
rel_clause(X^Rep1^and(Rep1, Rep2), S0, S3) :- rel_pron(S0, S1), vp(Y^X^Rep2, S1, S2), comp(X^Rep2^Rep2, S2, S3),!.
rel_clause(X^Rep^Rep, S, S).
adj_noun(X^Rep, S0, S1) :- adj(X^Rep, S0, S1),!.
adj_noun(X^Rep, S0, S1) :- noun(X^Rep, S0, S1),!.
adj_noun(X^and(Rep, Rep1), S0, S2) :- adj(X^Rep, S0, S1), noun(X^Rep1, S1, S2).
comp(X^Rep^Rep, S0, S1) :- adj(X^Rep, S0, S1).
comp(X^Rep^Rep, S0, S1) :- noun(X^Rep, S0, S1).
comp(X^Rep^Rep, [a|S1], S2) :- noun(X^Rep, S1, S2).

det(X^Restrictor^Scope^all(X, Restrictor->Scope), [every|T], T).
det(X^Restrictor^Scope^all(X, Restrictor->Scope), [all|T], T).
det(X^Restrictor^Scope^the(X, Restrictor & Scope), [the|T], T).
det(X^Restrictor^Scope^exist(X, Restrictor & Scope), [a|T], T).
det(X^Restrictor^Scope^exist(X, Restrictor & Scope), [an|T], T).
det(X^Restrictor^Scope^some(X, Scope), [someone|T], T).
det(X^Restrictor^Scope^all(X, Scope), [everyone|T], T).

noun(X^girl(X), [girl|T], T).
noun(X^doll(X), [doll|T], T).
noun(X^dancer(X), [dancer|T], T).
noun(X^apple(X), [apple|T], T).
noun(X^woman(X), [woman|T], T).
noun(X^man(X), [man|T], T).
noun(X^book(X), [book|T], T).

aux(be, [is|T], T).

adj(X^indian(X), [indian|T], T).
adj(X^fat(X), [fat|T], T).
adj(X^cute(X), [cute|T], T).
adj(X^mortal(X), [mortal|T], T).

proper_noun(john, [john|T], T).
proper_noun(mary, [mary|T], T).

t_verb(Y^X^knows(X, Y), [knows|T], T).
t_verb(Y^X^sees(X, Y), [sees|T], T).
t_verb(Y^X^likes(X, Y), [likes|T], T).
t_verb(Y^X^loves(X, Y), [loves|T], T).

in_tverb(X^cry(X), [cries|T], T).
in_tverb(X^cry(X), [cry|T], T).
in_tverb(X^laugh(X), [laughs|T], T).
in_tverb(X^laugh(X), [laugh|T], T).
in_tverb(X^walks(X), [walks|T], T).
in_tverb(X^walk(X), [walk|T], T).

rel_pron([who|T], T).
rel_pron([that|T], T).

prep([to|T], T).
prep([at|T], T).
prep([of|T], T).
