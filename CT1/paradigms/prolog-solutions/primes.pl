found_divisor(N, K) :-
    K * K =< N,
    (0 is N mod K -> true ;
     NextK is K + 2,
     found_divisor(N, NextK)).

prime(2).
prime(N) :-
    N > 2,
    1 is N mod 2,
    \+ found_divisor(N, 3).

composite(N) :-
    N > 1,
    \+ prime(N).

prime_divisors(1, []).
prime_divisors(N, Divisors) :-
    N > 1,
    factor(N, 2, [], Divisors).

factor(1, _, Res, Res) :- !.
factor(N, 2, Res, Divisors) :-
    !,
    (0 is N mod 2 ->
        NextN is N // 2,
        factor(NextN, 2, [2 | Res], Divisors)
        ;
        NextDiv is 3,
        factor(N, NextDiv, Res, Divisors)
    ).
factor(N, Div, Res, Divisors) :-
    Div * Div =< N,
    !,
    (0 is N mod Div, prime(Div) ->
        NextN is N // Div,
        factor(NextN, Div, [Div | Res], Divisors)
        ;
        NextDiv is Div + 2,
        factor(N, NextDiv, Res, Divisors)
    ).
factor(N, _, Res, Divisors) :-
    N > 1,
    !,
    (prime(N) ->
        reverse([N | Res], Divisors)
        ;
        reverse(Res, RevRes),
        append(RevRes, [N], Divisors)
    ).

power(1, 0) :- !.
power(N, M) :-
    N > 1,
    prime_divisors(N, Divs),
    find_max_occure(Divs, 0, M).

find_max_occure([], Max, Max).
find_max_occure([H|Tail], CurrentMax, Result) :-
    count_sequence(H, Tail, 1, Cnt, NewTail),
    (Cnt > CurrentMax ->
        NewMax = Cnt
        ;
        NewMax = CurrentMax
    ),
    find_max_occure(NewTail, NewMax, Result).

count_sequence(_, [], Cnt, Cnt, []).
count_sequence(H, [H|Tail], CurCnt, FinRes, NewTail) :-
    NewCount is CurCnt + 1,
    count_sequence(H, Tail, NewCount, FinRes, NewTail).
count_sequence(First, [Second|Tail], Cnt, Cnt, [Second|Tail]) :-
    First \= Second.