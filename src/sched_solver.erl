-module(sched_solver).
-compile([inline, native, {hipe, [o3]}]).
-export([
  solve/3,
  start/0
]).

% --------------------

solve(Function, InitialSteps, State) ->
  {Queue, Count} = append_steps(InitialSteps, gb_trees:empty(), 0),
  solve_astar(Function, Queue, Count, State).

% --------------------

append_steps(RankedSteps, Queue, Count) ->
  lists:foldl(
    fun({Rank, Step}, {QueueAcc, CountAcc}) ->
      {gb_trees:insert({Rank, CountAcc}, Step, QueueAcc), CountAcc+1}
    end,
    {Queue, Count},
    RankedSteps
  ).

% --------------------

solve_astar(Function, Queue, Count, State) ->
  case gb_trees:is_empty(Queue) of
    true -> State;
    _ ->
      {{BestRank, _}, BestStep, RemQueue} = gb_trees:take_smallest(Queue),
      case Function({BestRank, BestStep}, State) of
        {stop, NewState} -> NewState;
        {next, RankedSteps, NewState} ->
          {NewQueue, NewCount} = append_steps(RankedSteps, RemQueue, Count),
          solve_astar(Function, NewQueue, NewCount, NewState)
      end
  end.

% --------------------

adjacent_steps({_Rank, Step=[Pos|_]}) ->
  Next1 = [Pos+1|Step],
  Next2 = [Pos-1|Step],
  [{compute_rank(Next1), Next1},
   {compute_rank(Next2), Next2}].

compute_rank([Pos|_]) -> abs(10-Pos).

is_solution({_Rank, [Pos|_]}) -> Pos == 10.

start() ->
  InitialStep = [0],
  InitialState = [],
  FinalState = solve(
    fun(BestRankedStep, State) ->
      {NextSteps, NewState} = lists:foldl(
        fun(RankedStep, {NextAcc, StateAcc}) ->
          case is_solution(RankedStep) of
            true -> {NextAcc, [RankedStep|StateAcc]};
            _    -> {[RankedStep|NextAcc], StateAcc}
          end
        end,
        {[], State},
        adjacent_steps(BestRankedStep)
      ),
      if
        length(NewState) > 1 -> {stop, NewState};
        true -> {next, NextSteps, NewState}
      end
    end,
    [{compute_rank(InitialStep), InitialStep}],
    InitialState
  ),
  io:format("~p~n", [FinalState]),
  ok.
