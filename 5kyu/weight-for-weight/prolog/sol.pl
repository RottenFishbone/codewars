:- use_module(library(pairs)).

order_weight(S, R) :-
    split_string(S, ' ', '', ValStrs),
    msort(ValStrs, SortedValStrs),
    map_list_to_pairs(str_to_weight, SortedValStrs, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, SortedVals),
    atomics_to_string(SortedVals, ' ', R).

str_to_weight(S, R) :-
    atom_codes(S, Codes),
    findall(Num, (member(X, Codes), Num is X-48), Nums),
    sumlist(Nums, R).

