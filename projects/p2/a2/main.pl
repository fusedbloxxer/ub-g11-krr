:-dynamic
  eval_consequent/3,
  domain/2.


:-include('./utils.pl').
:-include('./io.pl').


transform_input(AntDegreeCurve, Membership):-
  atomic_list_concat([degree, Input, _], '_', AntDegreeCurve),
  user_input(Input, in, Value),
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


aggregate_consequents(X, Y):-
  findall(X -> Consequent -> Z, eval_consequent(Consequent, X, Z), E),
  findall(Z, eval_consequent(Consequent, X, Z), ConsequentsOutputs),
  max_list(ConsequentsOutputs, Y),
  log_format('max = ~2f: ', [Y]),
  write(E),
  nl.


defuzzify(Count, CentroidX):-
  domain(Start, End),
  intervals(Start, End, Count, PointsX),
  maplist(aggregate_consequents, PointsX, PointsY),
  maplist(multiply, PointsX, PointsY, Xs),
  maplist(multiply, PointsY, PointsY, Ys),
  sum_list(PointsY, SumY),
  sum_list(Xs, X),
  sum_list(Ys, Y),
  CentroidX is X / (SumY + 1e-12),
  CentroidY is Y / (SumY + 1e-12),
  log_format('PointsX  = '), write(PointsX), nl,
  log_format('PointsY  = '), write(PointsY), nl,
  log_format('Xs       = '), write(Xs), nl,
  log_format('Ys       = '), write(Ys), nl,
  log_format('Centroid = (~2f, ~2f)', [CentroidX, CentroidY]), nl.


solve(RuleList):-
  log_format('--> Read Rules'), nl,
  log_rule_list(RuleList), nl,
  log_format('--> Transform Input'), nl,
  transform_input_list(RuleList, AntMembership),
  log_rule_list(AntMembership), nl,
  log_format('--> Evaluate Antecedents'), nl,
  evaluate_antecedents_list(AntMembership, EvalAnt),
  log_rule_list(EvalAnt), nl,
  log_format('--> Evaluate Consequents'), nl,
  evaluate_consequents_list(EvalAnt, _),
  listing(eval_consequent),
  log_format('--> Aggregate Consequents (Predicate)'), nl,
  listing(aggregate_consequents),
  log_format('--> Aggregate Consequents + Defuzzify'), nl,
  defuzzify(10, Ans),
  log_format('--> Answer'), nl,
  user_input(Name, out, _),
  format('~s = ~2f', [Name, Ans]), nl,
  retractall(eval_consequent(_, _, _)).


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
  repeat,
  read_user_input,
  solve(RuleList),
  fail.