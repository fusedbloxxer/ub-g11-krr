:-dynamic
  n/1,
  can_fly/0,
  has_sharp_claws/0,
  has_colorful_feathers/0,
  user_rule/1,
  user_input/1,
  is_marked/1,
  user_question/1.


:-include('./utils.pl').
:-include('./io.pl').


% backward_chaining(KBRules, Qs).


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
  main(forward_chaining).
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