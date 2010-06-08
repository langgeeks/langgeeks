% reversi kata

% test data
% ---------------
position('.',0,0).
position('.',0,1).
position('.',0,2).
position('.',0,3).

position('.',1,0).
position('B',1,1).
position('W',1,2).
position('.',1,3).

position('.',2,0).
position('W',2,1).
position('B',2,2).
position('.',2,3).

position('.',3,0).
position('.',3,1).
position('.',3,2).
position('.',3,3).

% ---------------

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
