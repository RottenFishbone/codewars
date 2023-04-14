best_sum(T, K, Ls, Res) :-
    findall(Sum, (sum_n(K, Ls, 0, Sum), Sum =< T), Sums),
    max_list(Sums, Res).
best_sum(_, _, _, -1).

sum_n(N, _, Sum, Sum) :- N =< 0.
sum_n(N, [Head|List], Acc, Sum) :-
    N > 0,
    NextN is N-1,
    NextAcc is Acc+Head,
    sum_n(NextN, List, NextAcc, Sum).
sum_n(N, [_|List], Acc, Sum) :- N > 0, sum_n(N, List, Acc, Sum).
