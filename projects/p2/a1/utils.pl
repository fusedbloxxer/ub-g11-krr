wrap(X, [X]).
compose_rules(UserInput, UserRules, Rules):-
  maplist(wrap, UserInput, WrappedUserInput),
  append(WrappedUserInput, UserRules, Rules).


n(n(Literal), Literal):-
  !.
n(Literal, n(Literal)).

neg(X):-
  X = n(_).
pos(X):-
  not(neg(X)).