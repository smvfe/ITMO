map_build(MapList, MapTree) :-
    length(MapList, N),
    build_tree(N, MapList, MapTree, []).

build_tree(0, Res, nil, Res).
build_tree(N, List, node(K, V, Left, Right), Res) :-
    N > 0,
    LN is (N - 1) // 2,
    build_tree(LN, List, Left, [(K, V) | RList]),
    RN is N - LN - 1,
    build_tree(RN, RList, Right, Res).

map_get(node(K, V, _, _), K, V).
map_get(node(K, _, Left, _), Key, Value) :-
    Key < K,
    map_get(Left, Key, Value).
map_get(node(K, _, _, Right), Key, Value) :-
    Key > K,
    map_get(Right, Key, Value).