:-dynamic
  degree_service_poor/2,
  degree_service_good/2,
  degree_service_excellent/2,
  degree_food_rancid/2,
  degree_food_delicious/2,
  degree_tip_cheap/2,
  degree_tip_normal/2,
  degree_tip_generous/2,
  user_input/2,
  user_rule/3,
  eval_consequent/3.


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
    user_input(UserInputKey, _),
    write(UserInputKey), write('='), read(UserInputValue),
    (UserInputValue == stop -> clean_on_exit; true),
    retract(user_input(UserInputKey, _)),
    assertz(user_input(UserInputKey, UserInputValue)),
    fail
  ) ; true.
insert_user_inputs(FilePath):-
  see(FilePath),
  read(InputList),
  seen,
  ((member(X, InputList), assertz(user_input(X, null)), fail); true).
remove_user_inputs:-
  retractall(user_input(_, _)).


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
remove_user_rules:-
  retractall(user_rule(_, _, _)).


insert_degree_curves(FilePath):-
  see(FilePath),
  repeat,
  read(DegreeCurve),
  (DegreeCurve \== end_of_file -> (assertz(DegreeCurve), fail); (!, true)),
  seen.
remove_degree_curves:- % TODO: refactor this!
  retractall(degree_service_poor(_, _)),
  retractall(degree_service_good(_, _)),
  retractall(degree_service_excellent(_, _)),
  retractall(degree_food_rancid(_, _)),
  retractall(degree_food_delicious(_, _)),
  retractall(degree_tip_cheap(_, _)),
  retractall(degree_tip_normal(_, _)),
  retractall(degree_tip_generous(_, _)).


transform_input(AntDegreeCurve, Membership):-
  atomic_list_concat([degree, Input, _], '_', AntDegreeCurve),
  user_input(Input, Value),
  call(AntDegreeCurve, Value, Membership).
transform_input_item(Rule, OutMembership):-
  [Op, AntDegreeCurves, ConDegreeCurve] = Rule,
  maplist(transform_input, AntDegreeCurves, MembershipList),
  OutMembership = [Op, MembershipList, ConDegreeCurve].
transform_input_list(RuleList, OutRuleList):-
  maplist(transform_input_item, RuleList, OutRuleList).


evaluate_antecedents( or, MembershipList, Out):-
  max_list(MembershipList, Out).
evaluate_antecedents(and, MembershipList, Out):-
  min_list(MembershipList, Out).
evaluate_antecedents_item(Rule, OutRule):-
  [Op, AntMembership, ConDegreeCurve] = Rule,
  evaluate_antecedents(Op, AntMembership, Applicability),
  OutRule = [Applicability, ConDegreeCurve].
evaluate_antecedents_list(RuleList, OutRuleList):-
  maplist(evaluate_antecedents_item, RuleList, OutRuleList).


evaluate_consequents_item(Rule, OutRule):-
  [Degree, [ConDegreeCurve]] = Rule,
  atomic_list_concat([degree, Con, Pred], '_', ConDegreeCurve),
  atomic_list_concat([Con, Pred], '_', ConPred),
  assertz(eval_consequent(ConPred, X, Y):- (call(ConDegreeCurve, X, Y0), Y is min(Degree, Y0))),
  OutRule = ConPred.
evaluate_consequents_list(RuleList, OutRuleList):-
  maplist(evaluate_consequents_item, RuleList, OutRuleList).
remove_eval_consequents:-
  retractall(eval_consequent(_, _, _)).


aggregate_consequents(X, Y):-
  write('help'), nl,
  findall(X/Consequent/Z, eval_consequent(Consequent, X, Z), ConsequentsOutputHelper),
  findall(Z, eval_consequent(Consequent, X, Z), ConsequentsOutputs),
  write(ConsequentsOutputHelper), nl,
  max_list(ConsequentsOutputs, Y).


main:-
  main('./input.txt'),
  !.
main(RulesFile):-
  main('./degree.txt', RulesFile),
  !.
main(DegreeCurveFile, RulesFile):-
  main('./config.txt', DegreeCurveFile, RulesFile),
  !.
main(UserConfigFile, DegreeCurveFile, RulesFile):-
  insert_degree_curves(DegreeCurveFile),
  insert_user_inputs(UserConfigFile),
  insert_user_rules(RulesFile),
  read_user_rules(RuleList),
  % repeat,
  read_user_input,
  transform_input_list(RuleList, TransformedInput),
  evaluate_antecedents_list(TransformedInput, Applicability),
  evaluate_consequents_list(Applicability, Threshold),

  listing,
  write(         RuleList), nl,
  write( TransformedInput), nl,
  write(    Applicability), nl,
  write(        Threshold), nl,
  listing(eval_consequent), nl.

  % fail,
  % clean_on_exit.


clean_on_exit:-
  remove_eval_consequents,
  remove_user_rules,
  remove_user_inputs,
  remove_degree_curves.
  % halt.

