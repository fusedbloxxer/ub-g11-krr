:-dynamic
  n/1,
  is_big/0,
  can_fly/0,
  has_sharp_claws/0,
  has_colorful_feathers/0,
  user_rule/1,
  user_input/1,
  is_marked/1,
  user_question/1.


:-include('./utils.pl').
:-include('./io.pl').


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