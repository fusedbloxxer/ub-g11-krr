:-dynamic
  n/1,
  plays/0,
  has_tasks/0,
  boss_is_unhappy/0,
  misses_deadlines/0,
  user_rule/1,
  user_input/1,
  is_marked/1,
  user_question/1.


insert_user_input(Filepath):-
  see(Filepath),
  read(Input),
  ((member(Field, Input), assertz(user_input(Field)), fail); true),
  read(Question),
  assertz(user_question(Question)),
  seen.
read_user_input:-
  (
    user_input(Field),
    format('~s: ', [Field]),
    read(Value),
    (Value == stop -> halt; true),
    (Value == yes -> assertz(Field); assertz(n(Field))),
    fail
  ) ; true.
remove_user_input:-
  list_user_input(InputList),
  ((member(Input, InputList), retract(Input), fail); true).
list_user_input(InputList):-
  findall(R, (user_input(Field), ((Field, R=Field); (n(Field), R=n(Field)))), InputList).


insert_user_rules(Filepath):-
  see(Filepath),
  repeat,
  read(Rule),
  (Rule \== end_of_file -> (assertz(user_rule(Rule)), fail) ; !, true),
  seen.
list_user_rules(Rules):-
  findall(Rule, user_rule(Rule), Rules).


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


backward_chaining(_, Goals):-
  length(Goals, Len),
  Len =:= 0,
  !.
backward_chaining(KBRules, Goals):-
  [[G1] | _] = Goals,
  delete(Goals, [G1], OtherGoals),
  member(Rule, KBRules),
  member(G1, Rule),
  delete(Rule, G1, Negatives),
  maplist(n, Negatives, Positives_),
  maplist(wrap, Positives_, Positives),
  append(Positives, OtherGoals, Goals__),
  list_to_set(Goals__, Goals_),
  backward_chaining(KBRules, Goals_),
  format('Goal = ~s', G1), nl,
  write('Rule = '), write(Rule), nl,
  write('Goals = '), write(Goals_), nl, nl.


can_mark(Marked, Clause, Marked_):-
  member(E, Clause),
  pos(E),
  !,
  not(member(E, Marked)),
  delete(Clause, E, Negatives),
  maplist(n, Negatives, Positives),
  subset(Positives, Marked),
  union(Marked, [E], Marked_).
forward_chaining_(_, Goals, Marked):-
  flatten(Goals, Goals_),
  subset(Goals_, Marked),
  !.
forward_chaining_(KBRules, Goals, Marked):-
  member(Clause, KBRules),
  can_mark(Marked, Clause, Marked_),
  !,
  delete(KBRules, Clause, KBRules_),
  forward_chaining_(KBRules_, Goals, Marked_),
  write(Clause),
  nl.
forward_chaining(KBRules, Goals):-
  forward_chaining_(KBRules, Goals, []).


solve(Algorithm, KBRules, Question, 'YES'):-
  call(Algorithm, KBRules, Question),
  !.
solve(_, _, _, 'NO').


main:-
  main(backward_chaining).
main(Algorithm):-
  main(Algorithm, './input.txt').
main(Algorithm, RulesFile):-
  main(Algorithm, './config.txt', RulesFile).
main(Algorithm, InputConfigFile, RulesFile):-
  insert_user_input(InputConfigFile),
  insert_user_rules(RulesFile),
  list_user_rules(UserRules),
  repeat,
  read_user_input,
  list_user_input(UserInput),
  remove_user_input,
  compose_rules(UserInput, UserRules, KBRules),
  user_question(Question),
  write('UserInput = '), write(UserInput), nl,
  write('UserRules = '), write(UserRules), nl,
  write('KBRules = '), write(KBRules), nl,
  write('Question = '), write(Question), nl,
  solve(Algorithm, KBRules, Question, Answer),
  write('Answer = '), write(Answer), nl,
  fail.