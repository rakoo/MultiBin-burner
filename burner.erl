-module(burner).
-export([start/0, datetime_in_20_secs/0, datetime_in_10_secs/0,
    send_in_10_secs_doc/1, send_in_20_secs_doc/1]).

start() ->
  spawn_link(fun() -> init() end).

init() ->
  application:start(sasl),
  application:start(ibrowse),
  application:start(crypto),
  application:start(public_key),
  application:start(ssl),
  application:start(couchbeam),
  verify_connection(),
  nothing_to_do_loop().

nothing_to_do_loop() ->
  receive
    {_Pid, {start, Date = {{_,_,_},{_,_,_}}}} ->
      io:format("Received Date to burn : ~p~n", [iso8601date(Date)]),
      waiting_loop(Date)
  end.

waiting_loop(NextDate = {{_,_,_},{_,_,_}}) ->
  WaitTime = calendar:datetime_to_gregorian_seconds(NextDate) -
    calendar:datetime_to_gregorian_seconds(calendar:local_time()),
  receive
    {_Pid, {kill, Reason}} -> exit(Reason);
    {_Pid, {start, NewNextDate = {{_,_,_},{_,_,_}}}} ->
    case is_new_date(NewNextDate, NextDate) of
      true ->
        io:format("New Date : ~p~n", [iso8601date(NewNextDate)]),
        waiting_loop(NewNextDate);
      false ->
        io:format("Not a new date~n"),
        waiting_loop(NextDate)
    end;
    {Pid, Msg} -> Pid ! {self(), Msg}
  after WaitTime*1000 ->
      burning(NextDate)
  end.

burning(Date = {{_,_,_},{_,_,_}}) ->
  io:format("Burning ~p~n", [Date]),
  {ok, Cfg} = file:consult("credentials"),
  {Username, Password} = extract_credentials(Cfg),
  Server = couchbeam:server_connection("localhost", 5984, "",
    [{basic_auth, {Username, Password}}]),
  {ok, Db} = couchbeam:open_db(Server, "multibin-test", []),
  {ok, ViewResults} = couchbeam_view:fetch(Db, {"MultiBin",
      "expire_date"}, [{start_key, iso8601date(Date)}, descending,
      include_docs]),
  RelevantDocs = [{[{<<"_id">>,couchbeam_doc:get_id(X)},{<<"_rev">>,
          couchbeam_doc:get_rev(X)}]} 
    || {[_,_,_, {<<"doc">>,X}]} <- ViewResults],
  %{ok, Result} = couchbeam:delete_docs(Db, RelevantDocs),
  io:format("~p~n", [RelevantDocs]),
  io:format("Burnt ~p, waiting 3s~n", [Date]),
  timer:sleep(3000),
  io:format("Waited 3s~n"),
  nothing_to_do_loop().

send_in_20_secs_doc(Pid) ->
  Pid ! {self(), {start, datetime_in_20_secs()}}.

send_in_10_secs_doc(Pid) ->
  Pid ! {self(), {start, datetime_in_10_secs()}}.
  
iso8601date(Date = {{Y,Mo,D}, {H,Mn,S}}) ->
  {_,_,Micro} = now(),
  FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0BZ",
  IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S, Micro div 1000]),
  list_to_binary(IsoStr).

% Format of the cfg file:
% ```
% {username, "myusername"}.
% {password, "mypassword"}.
% ```
extract_credentials(Cfg) ->
  Username = proplist_get_value(username, Cfg),
  Password = proplist_get_value(password, Cfg),
  {Username, Password}.

proplist_get_value(Key,List)->
  case lists:keyfind(Key,1,List) of
    {_K,V}->
      V;
    false->
      io:format("key ~p wasn't found in list ~p~n", [Key, List]),
      undefined
  end.

datetime_in_10_secs() ->
  calendar:gregorian_seconds_to_datetime(
    calendar:datetime_to_gregorian_seconds(calendar:local_time()) + 10
  ).

datetime_in_20_secs() ->
  calendar:gregorian_seconds_to_datetime(
    calendar:datetime_to_gregorian_seconds(calendar:local_time()) + 20
  ).

is_new_date(NewDate,OldDate) ->
  calendar:datetime_to_gregorian_seconds(NewDate) <
  calendar:datetime_to_gregorian_seconds(OldDate).

verify_connection() ->
  {ok, Cfg} = file:consult("credentials"),
  {Username, Password} = extract_credentials(Cfg),
  Server = couchbeam:server_connection("localhost", 5984, "",
    [{basic_auth, {Username, Password}}]),
  {ok, _Version} = couchbeam:server_info(Server).
