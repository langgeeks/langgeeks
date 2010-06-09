% running tests

all :-
    test('test1.txt', [5:3,4:6,3:5,6:4]),
    test('test2.txt', [5:6,3:4]),
    test('test3.txt', [6:4,6:5]).

test(Filename, Expected) :-
    print('testing '),
    print(Filename),

    process(Filename),
    current_player(P),

    findall(R:C, legal_move(R,C,P), Expected) ->
    print(' pass.\n') ; print(' fail!\n').
