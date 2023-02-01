iterate(Start, End, _, []):-
  Start > End,
  !.
iterate(Start, End, _, [Start]):-
  Start =:= End,
  !.
iterate(Start, End, Step, [Start|Rest]):-
  Start < End,
  Next is Start + Step,
  iterate(Next, End, Step, Rest).


centers([], []):-
  !.
centers([_], []):-
  !.
centers([P0, P1|InRest], [P2|OutRest]):-
  P2 is (P1 + P0) / 2.0,
  centers([P1|InRest], OutRest).


multiply(X_i, Y_i, Z_i):-
  Z_i is X_i * Y_i.


intervals(_, _, 0, _):-
  !,
  fail.
intervals(Start, End, Count, OutPoints):-
  StepSize is (End - Start) / Count,
  iterate(Start, End, StepSize, OutPoints).


avg_list(InputList, OutAvg):-
  sum_list(InputList, Sum),
  length(InputList, Len),
  OutAvg is Sum / (Len + 1e-12).