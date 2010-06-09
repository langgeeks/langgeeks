% running tests

all :-
    test('test1.txt', [5:3,4:6,3:5,6:4]),
%    test('test2.txt', [5:6,3:4]),
    test('test2.txt', []),
    test('test3.txt', [6:4,6:5]).

test(Filename, Expected) :-
    print('testing '),
    print(Filename),

    run(Filename, Expected, Result)
    -> print(' pass.\n')
    ; ( print(' fail! Expected '),
	print(Expected),
	print(' but got '),
	print(Result),
	print('\n') ).

run(Filename, Expected, Result) :-
    process(Filename),
    current_player(P),

    !,

    findall(R:C, legal_move(R,C,P), Result),
    Expected = Result.
