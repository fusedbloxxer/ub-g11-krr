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
read_case(Filename, Case):-
  see(Filename),
  repeat,
  read(Case),
  (Case \== end_of_file -> true; !, seen, false).

read_file(Filename, List):-
  findall(Case, read_case(Filename, Case), List).

n(n(X), X):- !.
n(X, n(X)).

% Remove first apparition
del([], _, []):- !.
del([E|T], E, T):- !.
del([H|T], E, [H|Out]):- del(T, E, Out).

resolve(Clause1, Clause2, Res):-
  member(E1, Clause1),
  member(E2, Clause2),
  n(E2, NE2),
  E1 = NE2,
  del(Clause1, E1, Res1),
  del(Clause2, E2, Res2),
  append(Res1, Res2, Res).

res(KB):-
  member([], KB),
  !.
res(KB):-
  nth0(I1, KB, Clause1),
  nth0(I2, KB, Clause2),
  I2 > I1,
  resolve(Clause1, Clause2, Resolvent),
  not(member(Resolvent, KB)),
  append(KB, [Resolvent], NewKB),
  res(NewKB),
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
  !.

debug:-
  read_case('input.txt', Case), !, seen, res(Case), !.