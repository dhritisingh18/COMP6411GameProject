-module(player).
-export([init/3]).

init(Name, Credits, MasterPid) ->
  % Use os:system_time/0 to get the current time in nanoseconds for a unique seed
  Seed = erlang:phash2({self(), Name, os:system_time(nanosecond)}),
  rand:seed(exsplus, {Seed, Seed, Seed}),
  io:format("Player ~p started with ~p credits~n", [Name, Credits]),
  player_loop(Name, Credits, MasterPid).

player_loop(Name, 0, MasterPid) ->
  MasterPid ! {player_out, Name};
player_loop(Name, Credits, MasterPid) ->
  receive
    {start_game, GameId, Opponent} ->
      io:format("Player ~p starts game ~p with ~p~n", [Name, GameId, Opponent]),
      Result = random_rps(),
      Opponent ! {rps_move, GameId, Result},
      receive
        {rps_move, GameId, OpponentResult} ->
          Result = determine_winner(Result, OpponentResult),
          MasterPid ! {game_result, GameId, Name, Opponent, Result},
          NewCredits = if
                         Result =:= win -> Credits - 1;
                         true -> Credits
                       end,
          player_loop(Name, NewCredits, MasterPid)
      end;
    {game_request, _} ->
      player_loop(Name, Credits, MasterPid)
  end.

random_rps() ->
  Moves = [rock, paper, scissors],
  lists:nth(rand:uniform(length(Moves)), Moves).

determine_winner(rock, scissors) -> win;
determine_winner(paper, rock) -> win;
determine_winner(scissors, paper) -> win;
determine_winner(Move, Move) -> tie;
determine_winner(_, _) -> lose.
