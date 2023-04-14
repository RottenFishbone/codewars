play_pass(Str, Shift, Res) :- 
    string_codes(Str, StrCodes),
    transform_codes(StrCodes, Shift, false, [], PassCodes),
    string_codes(Res, PassCodes).

% On empty list, reverse and return accumulator.
transform_codes([], _, _, Acc, Res) :- reverse(Acc, Res).
% Recursively apply transform_code into an accumulator, flip-flopping Downcase
transform_codes([Code|Tail], Shift, Downcase, Acc, Res) :-
    transform_code(Code, Shift, Downcase, NewCode),
    append(Acc, [NewCode], NextAcc),
    (Downcase -> NextDowncase = false ; NextDowncase = true),
    transform_codes(Tail, Shift, NextDowncase, NextAcc, Res).

% Applies transformations as per the spec
transform_code(N, _, _, Res) :-             % Digit -- Complement to 9
    N >= 48, N =< 57, !,
    Res is 105-N. % (9 - (N-48) + 48)   Translate N to 0-9, sub then translate back
transform_code(N, Shift, Downcase, Res) :-  % Letter -- Circular right-shift
    N >= 65, N =< 90, !,
    ShiftedRaw is N + (Shift mod 26),
    (ShiftedRaw =< 90 -> 
        (Shifted is ShiftedRaw) ; 
        (Shifted is ShiftedRaw - 26)),
    (Downcase -> 
        (Res is Shifted+32) ; 
        (Res is Shifted)).
transform_code(Res, _, _, Res).             % Fallback -- No-op
