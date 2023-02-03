:-dynamic
  n/1,
  user_rule/1,
  user_input/1.


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
remove_user_rules:-
  list_user_rules(Rules),
  ((member(Rule, Rules), retract(user_rule(Rule)), fail); true).