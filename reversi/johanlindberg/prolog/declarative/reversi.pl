% reversi kata

% loads reversi state information from
% a text file.

process(File) :-
    retractall(position(_,_,_)),
    retractall(current_player(_)),

    open(File, read, In),
    get_char(In, Char1),

    R is 1,
    C is 1,
    process_stream(Char1, R, C, In),

    close(In).
 
process_stream(end_of_file, _, _, _) :- !.

% handle newlines

process_stream('\n', R, C, In) :-
    get_char(In, Char2),

    R1 is R + 1,
    process_stream(Char2, R1, 1, In).

% handle current player information

process_stream(Char, 9, 1, In) :-
    assertz(current_player(Char)),
    get_char(In, Char2),

    process_stream(Char2, 9, 2, In).

% handle board position info

process_stream(Char, R, C, In) :-
    assertz(position(Char,R,C)),
    get_char(In, Char2),

    C1 is C + 1,
    process_stream(Char2, R, C1, In).

% a position is a legal move if it
% is empty and has a chain to the
% right, left, up- or downwards of
% it.

legal_move(R,C,Player) :-
    position('.',R,C),

    C1 is C + 1,
    chain(R,C1,Player,0,1).

legal_move(R,C,Player) :-
    position('.',R,C),

    C1 is C - 1,
    chain(R,C1,Player,0,-1).

legal_move(R,C,Player) :-
    position('.',R,C),

    R1 is R + 1,
    chain(R1,C,Player,1,0).

legal_move(R,C,Player) :-
    position('.',R,C),

    R1 is R - 1,
    chain(R1,C,Player,-1,0).

legal_move(R,C,Player) :-
    position('.',R,C),

    C1 is C + 1,
    R1 is R + 1,
    chain(R1,C1,Player,1,1).

legal_move(R,C,Player) :-
    position('.',R,C),

    C1 is C + 1,
    R1 is R - 1,
    chain(R1,C1,Player,-1,1).

legal_move(R,C,Player) :-
    position('.',R,C),

    C1 is C - 1,
    R1 is R + 1,
    chain(R1,C1,Player,1,-1).

legal_move(R,C,Player) :-
    position('.',R,C),

    C1 is C - 1,
    R1 is R - 1,
    chain(R1,C1,Player,-1,-1).

% a chain consists of one
% opponent disc and one adjacent
% player disc or one opponent
% disc and one adjacent chain.

chain(R,C,Player,RD,CD) :-
    position(Opponent,R,C),
    Opponent \= '.',
    Opponent \= Player,

    C1 is C + CD,
    R1 is R + RD,

    !,

    ( position(Player,R1,C1) ;
      chain(R1,C1,Player,RD,CD) ).

% find moves, prints all legal moves
% given a text file containing game
% state.

find_moves(Filename) :-
    process(Filename),

    current_player(P),
    findall(R:C, legal_move(R,C,P), Result),

    print_moves(Result).

% prints the last move

print_moves([R:C|[]]) :-
    Code is R + 64,
    char_code(Char, Code),
    write(Char),write(C), nl.

% print moves

print_moves([R:C|Moves]) :-
    Code is R + 64,
    char_code(Char, Code),
    write(Char),write(C),write(','),
    print_moves(Moves).

% tests

all :-
    consult('../test.pl'),

    asserteq(test_find_moves, 'test1.txt', [5:3,4:6,3:5,6:4]),
    asserteq(test_find_moves, 'test2.txt', [5:6,3:4,3:6]),
    asserteq(test_find_moves, 'test3.txt', [6:4,6:5,6:3,6:6,6:7]).

test_find_moves(Filename, Result) :-
    process(Filename),
    current_player(P),

    !,

    findall(R:C, legal_move(R,C,P), Result).
