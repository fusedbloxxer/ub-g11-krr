:- dynamic
  literalCount/2,
  literalBalance/2.

read_case(Filename, Case):-
  see(Filename),
  repeat,
  read(Case),
  (Case \== end_of_file -> true; !, seen, fail).

read_file(Filename, List):-
  findall(X, read_case(Filename, X), List).

n(n(X), X):- !.
n(X, n(X)).

truth(n(X), X, 'false'):-
  !.
truth(X, X, 'true'):-
  !.

clause_count([]).
clause_count([Literal|Clause]):-
  literalCount(Count, Literal),
  retract(literalCount(Count, Literal)),
  NewCount is Count + 1,
  assert(literalCount(NewCount, Literal)),
  clause_count(Clause),
  !.
clause_count([Literal|Clause]):-
  assert(literalCount(1, Literal)),
  clause_count(Clause),
  !.

kb_count([]).
kb_count([Clause|KB]):-
  clause_count(Clause),
  kb_count(KB).

kb_balance([]).
kb_balance([Literal|Set]):-
  n(Literal, NegLiteral),
  literalCount(PosCount, Literal),
  (literalCount(NegCount, NegLiteral); NegCount = 0),
  DiffCount is PosCount - NegCount,
  abs(DiffCount, DiffAbs),
  assert(literalBalance(DiffAbs, Literal)),
  kb_balance(Set).

most_clauses(KB, P):-
  kb_count(KB),
  findall([Count, Literal], literalCount(Count, Literal), FreqMap),
  repeat,
  (retract(literalCount(_, _)) -> fail; !, true),
  sort(FreqMap, IncrFreqMap),
  reverse(IncrFreqMap, DecrFreqMap),
  [[_, P] | _] = DecrFreqMap.

most_balanced(KB, P):-
  kb_count(KB),
  findall(Literal, literalCount(_, Literal), LiteralSet),
  kb_balance(LiteralSet),
  findall([Balance, Literal], literalBalance(Balance, Literal), BalanceMap),
  sort(BalanceMap, IncrBalanceMap),
  [[_, P] | _] = IncrBalanceMap,
  repeat,
  (retract(literalCount(_, _)) -> fail; !, true),
  repeat,
  (retract(literalBalance(_, _)) -> fail; !, true).

write_one(Value):-
  tab(1),
  write(Value),
  write(';').
write_all([]).
write_all([Value|Rest]):-
  write_one(Value),
  write_all(Rest).
write_sol([]):-
  write('NO'),
  !.
write_sol(Solution):-
  write('YES - '),
  write('{'),
  write_all(Solution),
  tab(1),
  write('}').

dot([], _, []).
dot([Clause|KB], P, Other):-
  dot(KB, P, Other),
  member(P, Clause),
  !.
dot([Clause|KB], P, [FClause|Other]):-
  dot(KB, P, Other),
  n(P, NP),
  member(NP, Clause),
  delete(Clause, NP, FClause),
  !.
dot([Clause|KB], P, [Clause|Other]):-
  dot(KB, P, Other),
  !.

dp_([], _, []):-
  !,
  true.
dp_(KB, _, _):-
  member([], KB),
  !,
  fail.
dp_(KB, Strategy, [Var/State|Res]):-
  call(Strategy, KB, P),
  dot(KB, P, PKB),
  dp_(PKB, Strategy, Res),
  write('KB: '),
  write(KB),
  nl,
  write('P: '),
  write(P),
  nl,
  write('PKB: '),
  write(KB),
  nl,
  !,
  truth(P, Var, State).
dp_(KB, Strategy, [Var/State|Res]):-
  call(Strategy, KB, P),
  n(P, NP),
  dot(KB, NP, PKB),
  dp_(PKB, Strategy, Res),
  write('KB: '),
  write(KB),
  nl,
  write('NP: '),
  write(NP),
  nl,
  write('PKB: '),
  write(KB),
  nl,
  !,
  truth(NP, Var, State).
dp(KB, Strategy, Sol):-
  dp_(KB, Strategy, Sol),
  !,
  write_sol(Sol),
  nl,
  nl.
dp(_, _, _):-
  write_sol([]),
  nl,
  nl,
  fail.

solve([], _, []).
solve([Case|Rest], Strategy, [Sol|Sols]):-
  write('CASE: '),
  write(Case),
  nl,
  dp(Case, Strategy, Sol),
  solve(Rest, Strategy, Sols),
  !.
solve([_|Rest], Strategy, [[]|Sols]):-
  solve(Rest, Strategy, Sols).

write_fin([]).
write_fin([Entry|Sol]):-
  write_sol(Entry),
  nl,
  write_fin(Sol).
write_file(Filename, Sol):-
  tell(Filename),
  write_fin(Sol),
  told.

main(Strategy):-
  read_file('input.txt', KBs),
  solve(KBs, Strategy, Sols),
  write_file('output.txt', Sols).