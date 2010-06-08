% reversi kata

position('.',0,0).
position('B',0,1).
%position('B',0,2).
position('W',0,2).

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
