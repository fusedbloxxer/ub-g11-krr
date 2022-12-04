% ----------- helpers -----------
% dynamic rules
% assert, asserta, assertz, retract
% assert of rule => use (rule:-...)
% name(X, [...])
% functor(Term, F, N) -> F = functor name, N = arity
% arg(N, Term, A) -> nth argument A, starting from 1
% call(P) - gol dinamic cu variabile
% repeat - force backtracking on failure using alternate branches
% trace, notrace, spy(P), nospy(P) - P=predicate
% var, nonvar, atom, integer, atomic
% see(X), tell(X), seen, told, end_of_file
% get, get0, put - character based
% read, write, tab(N), nl
% ----------- helpers -----------
:- dynamic
  literalCount/2,
  literalBalance/2.

% open file, read a row/term, then yield it
% if eof is encountered stop backtracing to repeat, close file
% and fail to avoid outputting a eof to the caller
read_case(Filename, Case):-
  see(Filename),
  repeat,
  read(Case),
  (Case \== end_of_file -> true; !, seen, fail).

% collect all cases from a file and output them as a list
read_file(Filename, List):-
  findall(X, read_case(Filename, X), List).

% Apply Negation
n(n(X), X):- !.
n(X, n(X)).

% Truth Value
truth(n(X), X, 'false'):-
  !.
truth(X, X, 'true'):-
  !.

% Cache Frequency
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

% Cache Frequency For All
kb_count([]).
kb_count([Clause|KB]):-
  clause_count(Clause),
  kb_count(KB).

% Add Balance Cache For All
kb_balance([]).
kb_balance([Literal|Set]):-
  n(Literal, NegLiteral),
  literalCount(PosCount, Literal),
  (literalCount(NegCount, NegLiteral); NegCount = 0),
  DiffCount is PosCount - NegCount,
  abs(DiffCount, DiffAbs),
  assert(literalBalance(DiffAbs, Literal)),
  kb_balance(Set).

% First Strategy
most_clauses(KB, P):-
  kb_count(KB),
  findall([Count, Literal], literalCount(Count, Literal), FreqMap),
  repeat,
  (retract(literalCount(_, _)) -> fail; !, true),
  sort(FreqMap, IncrFreqMap),
  reverse(IncrFreqMap, DecrFreqMap),
  [[_, P] | _] = DecrFreqMap.

% Second Strategy
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

% Pretty Format
write_one(Value):-
  tab(1),
  write(Value),
  write(';').
write_all([]).
write_all([Value|Rest]):-
  write_one(Value),
  write_all(Rest).
write_sol(Solution):-
  write('{'),
  write_all(Solution),
  tab(1),
  write('}').

% Dot Operation
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

dp([], _, []):-
  !,
  true.
dp(KB, _, _):-
  member([], KB),
  !,
  fail.
dp(KB, Strategy, [Var/State|Res]):-
  call(Strategy, KB, P),
  dot(KB, P, PKB),
  dp(PKB, Strategy, Res),
  !,
  truth(P, Var, State).
dp(KB, Strategy, [Var/State|Res]):-
  call(Strategy, KB, P),
  n(P, NP),
  dot(KB, NP, PKB),
  dp(PKB, Strategy, Res),
  !,
  truth(NP, Var, State).
dp(KB, Strategy):-
  dp(KB, Strategy, Solution),
  !,
  write('YES - '),
  write_sol(Solution),
  nl.
dp(_, _):-
  write('NO'),
  nl,
  fail.

solve([], _).
solve([Case|Rest], Strategy):-
  dp(Case, Strategy),
  !,
  solve(Rest, Strategy).
solve([_|Rest], Strategy):-
  solve(Rest, Strategy).

main(Strategy):-
  tell('output.txt'),
  read_file('input.txt', KBs),
  solve(KBs, Strategy),
  told.