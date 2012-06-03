-module(burner).
-export([start/0]).

start() ->
  application:start(sasl),
  application:start(ibrowse),
  application:start(crypto),
  application:start(public_key),
  application:start(ssl),
  application:start(couchbeam),
  {ok, Cfg} = file:consult("credentials"),
  {Username, Password} = extract_credentials(Cfg),
  Server = couchbeam:server_connection("localhost", 5984, "",
    [{basic_auth, {Username, Password}}]),
  {ok, Db} = couchbeam:open_db(Server, "multibin-test", []),
  {ok, ViewResults} = couchbeam_view:fetch(Db, {"MultiBin",
      "expire_date"}, [{start_key, iso8601date()}, descending,
      include_docs]),
  RelevantDocs = [{[{<<"_id">>,couchbeam_doc:get_id(X)},{<<"_rev">>,
          couchbeam_doc:get_rev(X)}]} 
    || {[_,_,_, {<<"doc">>,X}]} <- ViewResults],
  {ok, Result} = couchbeam:delete_docs(Db, RelevantDocs),
  io:format("~p~n", [Result]).
  
iso8601date() ->
  {{Y,Mo,D}, {H,Mn,S}} = erlang:localtime(),
  {_,_,Micro} = now(),
  FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0BZ",
  IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S, Micro div 1000]),
  list_to_binary(IsoStr).

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
