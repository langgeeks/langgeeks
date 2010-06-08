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

legal_move(R,C,Player) :-
    position('.',R,C), % position must be empty
    C1 is C + 1,
    rchain(R,C1,Player).

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
