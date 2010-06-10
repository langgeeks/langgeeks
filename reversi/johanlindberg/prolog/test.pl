% running tests

all :-
    test(run, 'test1.txt', [5:3,4:6,3:5,6:4]),
    test(run, 'test2.txt', [5:6,3:4]),
    test(run, 'test3.txt', [6:4,6:5]).

test(Functor, Args, Expected) :-
    Goal =.. [Functor,Args,Result],

    write('testing '),
    writeq(Goal),

    call(Goal),

    Expected \= Result
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
