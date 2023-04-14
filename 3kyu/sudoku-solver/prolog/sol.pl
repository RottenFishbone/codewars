:- use_module(library(clpfd)). 

%-------------------------
% sudoku(+Puzzle, -Solution)
%
% Fills in 0's within Puzzle using constraints of Sudoku rules.
% Puzzle is a 9x9 2D list with values from 0..9, where 0 are variables.
%
% Solution is a 9x9 2D list with all cells being concrete values 1..9.
% ------------------------
sudoku(Puzzle, Solution) :-
    %% Apply Constraints
    % Unbind 0's to be free variables
    maplist(maplist(zeros_to_unbound), Puzzle, Board),
    % Constrain variable domain to 1..9
    maplist(list_ins(1..9), Board),     
    % Constrain cells to be all different in row
    maplist(all_distinct, Board),
    % Constrain cells to be all different in col
    transpose(Board, XBoard),
    maplist(all_distinct, XBoard),
    % Constrain 3x3 regions
    constr_regions(Board),
    % Solve
    maplist(labeling([]), Board),
    Solution = Board.


% Unbinds 0, changing to a free variable.
zeros_to_unbound(0, _) :- !.   % Zeros are unbound, no backtracking   
zeros_to_unbound(N, N).        % All other numbers are unchanged

% Reversed arguments for `ins` for use in maplist
list_ins(Range, List) :- List ins Range.

% Constrains 3x3 regions to contain all unique values within the board
constr_regions([]).
constr_regions([R1, R2, R3|Rs]) :-  % Take 3 rows at a time
    constr_regions(R1, R2, R3),     % Apply constraints to each row
    constr_regions(Rs).             % Repeat until out of rows

% Take 3 items from each row, constrain the entire set to be distinct
% Repeat until all columns are exhausted
constr_regions([], [], []).
constr_regions([A1,A2,A3|As], [B1,B2,B3|Bs], [C1,C2,C3|Cs]) :-
    all_distinct([A1,A2,A3,B1,B2,B3,C1,C2,C3]),
    constr_regions(As, Bs, Cs).     


