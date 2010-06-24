% running tests

all :-
    asserteq(run, 'test1.txt', [5:3,4:6,3:5,6:4]),
    asserteq(run, 'test2.txt', [5:6,3:4,3:6]),
    asserteq(run, 'test3.txt', [6:4,6:5,6:3,6:6,6:7]).

asserteq(Functor, Args, Expected) :-
    test(Functor, Args, Expected, \=).

assertneq(Functor, Args, Expected) :-
    test(Functor, Args, Expected, =).

test(Functor, Args, Expected, Cmp) :-
    Goal =.. [Functor,Args,Result],

    write('testing '),
    writeq(Goal),

    call(Goal),

    Comparison =.. [Cmp,Expected,Result],

    call(Comparison)
    -> ( write(' fail! Expected '),
	 write(Expected),
	 write(' but got '),
	 write(Result), nl )
    ;  ( write(' pass.'), nl ).

run(Filename, Result) :-
    process(Filename),
    current_player(P),

    !,

    findall(R:C, legal_move(R,C,P), Result).
