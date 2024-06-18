-module(game).
-export([start/1]).

start([PlayerFile]) ->
  {ok, PlayerInfo} = file:consult(PlayerFile),
  io:format("Starting game with players: ~p~n", [PlayerInfo]),
  MasterPid = self(),
  spawn_players(PlayerInfo, MasterPid),
  game_loop(PlayerInfo).

spawn_players([], _MasterPid) ->
  ok;
spawn_players([{Name, Credits} | Rest], MasterPid) ->
  PlayerPid = spawn(player, init, [Name, Credits, MasterPid]),
  register(Name, PlayerPid),
  spawn_players(Rest, MasterPid).

game_loop(Players) ->
  receive
    {game_request, From, To} ->
      io:format("+ New game request from ~p to ~p~n", [From, To]),
      GameId = make_ref(),
      From ! {start_game, GameId, To},
      To ! {start_game, GameId, From},
      game_loop(Players);
    {game_result, GameId, From, To, Result} ->
      io:format("$ Game ~p: ~p vs ~p = ~p~n", [GameId, From, To, Result]),
      game_loop(Players);
    {player_out, Name} ->
      io:format("- Player ~p is out~n", [Name]),
      game_loop(lists:delete(Name, Players))
  end.
