% running tests

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
