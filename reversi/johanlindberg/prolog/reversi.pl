% reversi kata

process(File) :-
    open(File, read, In),
    get_char(In, Char1),

    R is 1,
    C is 1,
    process_stream(Char1, R, C, In),

    close(In).
 
process_stream(end_of_file, _, _, _) :- !.
process_stream(Char, R, C, In) :-
    print(position(Char,R,C)),
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
    rchain(R,C1,Player).

legal_move(R,C,Player) :-
    position('.',R,C),

    C1 is C - 1,
    lchain(R,C1,Player).

legal_move(R,C,Player) :-
    position('.',R,C),

    R1 is R + 1,
    dchain(R1,C,Player).

legal_move(R,C,Player) :-
    position('.',R,C),

    R1 is R - 1,
    uchain(R1,C,Player).

% an (r)chain consists of one
% opponent disc and one adjacent
% player disc or one opponent
% disc and one adjacent (r)chain.

rchain(R,C,Player) :-
    position(Opponent,R,C),
    Opponent \= '.',
    Opponent \= Player,

    C1 is C + 1,
    position(Player,R,C1).

rchain(R,C,Player) :-
    position(Opponent,R,C),
    Opponent \= '.',
    Opponent \= Player,

    C1 is C + 1,
    rchain(R,C1,Player).

% left chain

lchain(R,C,Player) :-
    position(Opponent,R,C),
    Opponent \= '.',
    Opponent \= Player,

    C1 is C - 1,
    position(Player,R,C1).

lchain(R,C,Player) :-
    position(Opponent,R,C),
    Opponent \= '.',
    Opponent \= Player,

    C1 is C - 1,
    lchain(R,C1,Player).

% downwards chain

dchain(R,C,Player) :-
    position(Opponent,R,C),
    Opponent \= '.',
    Opponent \= Player,

    R1 is R + 1,
    position(Player,R1,C).

dchain(R,C,Player) :-
    position(Opponent,R,C),
    Opponent \= '.',
    Opponent \= Player,

    R1 is R + 1,
    dchain(R1,C,Player).

% upwards chain

uchain(R,C,Player) :-
    position(Opponent,R,C),
    Opponent \= '.',
    Opponent \= Player,

    R1 is R - 1,
    position(Player,R1,C).

uchain(R,C,Player) :-
    position(Opponent,R,C),
    Opponent \= '.',
    Opponent \= Player,

    R1 is R - 1,
    uchain(R1,C,Player).
