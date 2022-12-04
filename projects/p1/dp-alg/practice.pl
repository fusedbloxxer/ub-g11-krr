% ['filename']. or consult('filename').
% help(function).
% halt. - exit swipl
% make. - reload the consulted files
% use ; for more answers, use . to stop answer search.


conc([], X, X).
conc([H1|T1], L2, [H1|F]):-
  conc(T1, L2, F).

is_sublist(L1, L2):-
  conc(_, Y, L2),
  conc(L1, _, Y).

is_in(X, [X|_]).
is_in(X, [_|T]):-
  is_in(X, T).

is_in(X, L, Pos):-
  is_in(X, L, 0, Pos).
is_in(X, [X|_], Pos, Pos).
is_in(X, [_|T], Iter, Pos):-
  Index is Iter + 1,
  is_in(X, T, Index, Pos).

del(X, [X|T], T).
del(X, [Y|T], [Y|O]):-
  X \== Y,
  del(X, T, O).

perm([], []).
perm(L, [X|O]):-
  del(X, L, Y),
  perm(Y, O).

elen([], 0).
elen([_|T], Len):-
  elen(T, OldLen),
  Len is OldLen + 1.
olen(List):-
  not(elen(List)).
elen(List):-
  elen(List, Y),
  Y mod 2 =:= 0.

rev([], X, X).
rev([H|T], P, R):-
  L = [H|P],
  rev(T, L, O),
  R = O.
rev(X, Y):-
  rev(X, [], Y).

palindrome(List):-
  rev(List, RList),
  is_sublist(RList, List).

append(X, [], [X]).
append(X, [H|T], [H|O]):-
  append(X, T, O).

sft([H|T], L2):-
  append(H, T, L2).

means(0, zero).
means(1, one).
means(2, two).
means(3, three).
means(4, four).
means(5, five).
means(6, six).
means(7, seven).
means(8, eight).
means(9, nine).

translate([], []).
translate([H|T], [TR|Partial]):-
  translate(T, Partial),
  means(H, TR).

after([], _, []):-!.
after(List, 0, List):-!.
after([_|T], Pos, Seq):-
  Next is Pos - 1,
  after(T, Next, Seq).

subset([], []).
subset(Set, [X|Partial]):-
  print(Set),
  is_in(X, Set, Pos),
  Next is Pos + 1,
  after(Set, Next, Seq),
  subset(Seq, Partial).

max(X, Y, Y):- X < Y, !.
max(X, _, X).

member(X, [X|_]).
member(X, [_|T]):- member(X, T).

add_dupl(X, L, L):-
  is_in(X, L), !.
add_dupl(X, L, [X|L]).

class(Number, positive):- Number > 0, !.
class(0, zero):- !.
class(Number, negative):- Number <0, !.

% non-cut
ssplit([], [], []):- !.
ssplit([H|T], [H|Pos], Neg):-
  (class(H, positive);class(H,zero)),
  !,
  ssplit(T, Pos, Neg),
  !.
ssplit([H|T], Pos, [H|Neg]):-
  class(H, negative),
  !,
  ssplit(T, Pos, Neg),
  !.

included([], _, []).
included([CH|CT], RuledOut, [CH|Partial]):-
  included(CT, RuledOut, Partial),
  not(member(CH, RuledOut)), !.
included([CH|CT], RuledOut, Partial):-
  included(CT, RuledOut, Partial),
  member(CH, RuledOut).

difference(Set1, Set2, SetDifference):-
  included(Set1, Set2, SetDifference).

% big brain
unifiable([], _, []):- !.
unifiable([H1|T1], Term, Prev):-
  not(H1 = Term),
  unifiable(T1, Term, Prev),
  !.
unifiable([H1|T], Term, [H1|Prev]):-
  unifiable(T, Term, Prev),
  !.

% see(X), tell(X), seen, told, end_of_file
% get, get0, put - character based
% read, write, tab(N), nl

writelist([]).

writelist([H|T]):-
  write(H),
  nl,
  writelist(T).

writeline([]).
writeline([H|T]):-
  write(H),
  tab(1),
  writeline(T).
writematrix([]).
writematrix([H|T]):-
  writeline(H),
  nl,
  writematrix(T).

stars(X):- X =< 0, !.
stars(X):- X > 0, write(*), Y is X - 1, stars(Y).

bars([]).
bars([H|T]):-
  stars(H),
  nl,
  bars(T).

process(end_of_file, _):-
  !.
process(Item, Supplier):-
  Item = item(_, _, _, Supplier),
  write(Item), nl,
  filter_entries(Supplier),
  !.
process(_, Supplier):-
  filter_entries(Supplier),
  !.

filter_entries(Supplier):-
  read(Item),
  process(Item, Supplier).

makefile(Supplier):-
  write('Supplier'), tab(1), write(Supplier), nl,
  filter_entries(Supplier).

take(end_of_file, _):-
  !.
take(X, Term):-
  not(X = Term),
  write('Unmatched - Term: '),
  write(Term),
  tab(1),
  write('X: '),
  write(X),
  nl,
  findTerm(Term),
  !.
take(X, Term):-
  write('Match - Term: '),
  write(Term),
  tab(1),
  write('X: '),
  write(X),
  nl,
  findTerm(Term),
  !.

findTerm(Term):-
  read(X),
  take(X, Term).

dorest(46):-
  !.

dorest(32):-
  get(C),
  (C == 44 -> true ; put(32)),
  dorest(C),
  !.

dorest(44):-
  put(44),
  put(32),
  skip([44,32], C),
  dorest(C),
  !.

dorest(C):-
  put(C),
  squeeze.

squeeze:-
  get0(C),
  dorest(C).

skip(List, O):-
  get0(C),
  (not(member(C, List)), O = C;skip(List, O)),
  !.

starts(Atom, Character):-
  name(Atom, [H|_]),
  name(FirstChar, [H]),
  Character == FirstChar.

plural(Noun, Plural):-
  name(Noun, CharList),
  name(s, [C]),
  append(C, CharList, NewList),
  !,
  name(Plural, NewList).

search(KeyWord, Sentence):-
  name(KeyWord, KeyWordList),
  name(Sentence, SentenceList),
  conc(_, L2, SentenceList),
  conc(KeyWordList, _, L2),
  !.

% var, nonvar, atom, integer, atomic

walk(+(T, H)):-
  walk(T),
  print(H),
  !.
walk(T):-
  print('last'),
  print(T).

ins_item(H, PTups, Tups):-
  not(member([H, _], PTups)),
  append([H, 1], PTups, Tups),
  !.
inc_item(_, [], []):-
  !.
inc_item(H, [[H, Count]|T], [[H, NewCount]|T]):-
  NewCount is Count + 1,
  !.
inc_item(H, [Y|T], [Y|PTups]):-
  inc_item(H, T, PTups),
  !.
add_item(H, PTups, Tups):-
  ins_item(H, PTups, Tups);
  inc_item(H, PTups, Tups).

simplify(T, 0, Tups):-
  atom(T),
  ins_item(T, [], Tups),
  !.
simplify(T, T, []):-
  integer(T),
  !.
simplify(+(T, H), Num, Tups):-
  simplify(T, PNum, Tups),
  integer(H),
  Num is PNum + H,
  !.
simplify(+(T, H), Num, Tups):-
  simplify(T, Num, PTups),
  atom(H),
  (
    H == + -> (Tups = PTups)
            ; add_item(H, PTups, Tups)
  ),
  !.

expr([], -).
expr([[H, C]|T], E):-
  expr(T, P),
  (
    P == - ->
      (C == 1 -> E = H ; E = C * H)
      ;
      (C == 1 -> E = H + P ; E = C * H + P)
  ),
  !.

simplify(E, O):-
  simplify(E, Num, Tups),
  expr(Tups, ETups),
  O = ETups + Num.

decompose(X):-
  X =.. [Head | Tail],
  print(Head),
  print(Tail).

ground_(Term):-
  var(Term),
  fail,
  !.
ground_(Term):-
  atomic(Term),
  !.
ground_(Term):-
  nonvar(Term),
  Term =.. [F | P],
  nonvar(F),
  grounds_(P).

grounds_([]):-
  !.
grounds_([P|T]):-
  ground_(P),
  grounds_(T).

main:-
  see('spaces-commas.txt'),
  squeeze,
  seen.

% name(X, [...])
% functor(Term, F, N) -> F = functor name, N = arity
% arg(N, Term, A) -> nth argument A, starting from 1


copy(Term, Copy):-