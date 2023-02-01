:-dynamic
  user_input/3,
  user_rule/3.


log_level('Internal').


pair2degree(Pair, OutDegreeCurve):-
  not(is_list(Pair)),
  !,
  FunBase/Predicate = Pair,
  atomic_list_concat([degree, FunBase, Predicate], '_', OutDegreeCurve).
pair2degree([], []).
pair2degree([Pair|PairTail], [OutDegreeCurve|Rest]):-
  pair2degree(Pair, OutDegreeCurve),
  pair2degree(PairTail, Rest).


read_user_input:-
  (
    user_input(UserInputKey, in, _),
    write(UserInputKey), write('='), read(UserInputValue),
    (UserInputValue == stop -> halt; true),
    retract(user_input(UserInputKey, in, _)),
    assertz(user_input(UserInputKey, in, UserInputValue)),
    fail
  ) ; true.
insert_user_inputs(FilePath):-
  see(FilePath),
  read(InputList),
  seen,
  ((member(X/InOut, InputList), assertz(user_input(X, InOut, null)), fail); true).


read_user_rules(RuleList):-
  findall([X, Y, Z], user_rule(X, Y, Z), RuleList).
insert_user_rules(FilePath):-
  see(FilePath),
  repeat,
  read(InputList),
  (
    InputList \== end_of_file ->
    (
      [Op, Ant, Con] = InputList,
      pair2degree(Ant, DegreeCurveAnt),
      pair2degree(Con, DegreecurveCon),
      assertz(user_rule(Op, DegreeCurveAnt, DegreecurveCon)),
      fail
    ) ; (!, true)
  ),
  seen.


insert_degree_curves(FilePath):-
  see(FilePath),
  read(Domain),
  assertz(Domain),
  repeat,
  read(DegreeCurve),
  (DegreeCurve \== end_of_file -> (assertz(DegreeCurve), fail); (!, true)),
  seen.


log_format(Format):-
  !,
  log_format(Format, []).
log_format(Format, Args):-
  log_level(Degree),
  !,
  log_format(Degree, Format, Args).
log_format(Format, Args):-
  !,
  format(Format, Args).
log_format(Degree, Format, Args):-
  format('[~s] ', Degree),
  format(Format, Args).

log_rule_list([], _).
log_rule_list([Rule|Rest], Index):-
  nl,
  log_format('  ~d: ', Index),
  write(Rule),
  NextIndex is Index + 1,
  log_rule_list(Rest, NextIndex).
log_rule_list(RuleList):-
  log_format('rules = ['),
  log_rule_list(RuleList, 0),
  nl,
  log_format(']').