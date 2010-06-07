% reversi kata

position('.',0,0).
position('B',0,1).
position('W',0,2).

legal_move(R,C,Player) :-
    position('.',R,C),
    position(Opponent,R,C+1),
    Opponent \= Player,
    position(Player,R,C+2).
