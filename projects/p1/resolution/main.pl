read_case(Filename, Case):-
  see(Filename),
  repeat,
  read(Case),
  (Case \== end_of_file -> true; !, seen, false).

read_file(Filename, List):-
  findall(Case, read_case(Filename, Case), List).

n(n(X), X):- !.
n(X, n(X)).

unique([], _):- !.
unique(Dup, NDup):-
  setof(E, member(E, Dup), NDup).

memberOf(_, []):-
  !,
  fail.
memberOf(Item, [Elem|_]):-
  Item == Elem,
  !.
memberOf(Item, [_|Rest]):-
  memberOf(Item, Rest).

subsetOf([], _):- !.
subsetOf([Elem|Rest], Set):-
  subsetOf(Rest, Set),
  memberOf(Elem, Set).

pure_clause(Clause, KB):-
  member(Literal, Clause),
  flatten(KB, KBLiterals),
  n(Literal, NLiteral),
  not(member(NLiteral, KBLiterals)),
  !.

tautology(Clause):-
  member(E1, Clause),
  member(E2, Clause),
  n(E2, NE2),
  E1 == NE2,
  !.

subsumed_clause(Clause, KB, vars):-
  member(KBClause, KB),
  subset(KBClause, Clause),
  !.
subsumed_clause(Clause, KB, prop):-
  member(KBClause, KB),
  subsetOf(KBClause, Clause),
  !.

should_add([], _, _):- !.
should_add(Clause, KB, When):-
  not(tautology(Clause)),
  not(pure_clause(Clause, KB)),
  not(subsumed_clause(Clause, KB, When)).

resolve(Clause1, Clause2, Res):-
  member(E1, Clause1),
  member(E2, Clause2),
  n(E2, NE2),
  E1 = NE2,
  delete(Clause1, E1, Res1),
  delete(Clause2, E2, Res2),
  append(Res1, Res2, Concat),
  unique(Concat, Res).

res_(KB):-
  member([], KB),
  !.
res_(KB):-
  nth0(I1, KB, HiddenClause1),
  nth0(I2, KB, HiddenClause2),
  I2 > I1,
  copy_term(HiddenClause1, Clause1),
  copy_term(HiddenClause2, Clause2),
  % write('RC1: '),
  % print(Clause1),
  % tab(1),
  % write('RC2: '),
  % print(Clause2),
  % nl,
  resolve(Clause1, Clause2, Resolvent),
  % write(' P: '),
  % print(KB),
  % nl,
  % write('C1: '),
  % print(Clause1),
  % nl,
  % write('C2: '),
  % print(Clause2),
  % nl,
  % write('RE: '),
  % print(Resolvent),
  % nl,
  should_add(Resolvent, KB, vars),
  append(KB, [Resolvent], NewKB),
  % write('AC: '),
  % print(Resolvent),
  % nl,
  % write('KB: '),
  % print(NewKB),
  % nl,
  res_(NewKB),
  write(' P: '),
  print(KB),
  nl,
  write('KB: '),
  print(NewKB),
  nl,
  write('C1: '),
  print(Clause1),
  nl,
  write('C2: '),
  print(Clause2),
  nl,
  write('RE: '),
  print(Resolvent),
  nl,
  nl.

keep([], OptimKB, OptimKB):-
  !.
keep([Clause|KB], OptimKB, Res):-
  copy_term(Clause, CopyClause1),
  append(KB, OptimKB, PartialKB),
  should_add(Clause, PartialKB, prop),
  keep(KB, [CopyClause1|OptimKB], Res),
  !.
keep([_|KB], OptimKB, Res):-
  keep(KB, OptimKB, Res),
  !.

res(KB):-
  write('KB: '), write(KB), nl,
  findall(UniqClause, (member(DupClause, KB), unique(DupClause, UniqClause)), UniqKB),
  write('UniqueKB: '), write(UniqKB), nl,
  keep(UniqKB, [], OptimKB),
  write('OptimKB: '), write(OptimKB), nl,
  res_(OptimKB).

solve([]).
solve([Case|Rest]):-
  res(Case),
  !,
  write('UNSATISFIABLE'),
  nl,
  solve(Rest).
solve([_|Rest]):-
  write('SATISFIABLE'),
  nl,
  solve(Rest).

main:-
  % tell('output.txt'),
  read_file('input.txt', KBs),
  solve(KBs).
  % told.