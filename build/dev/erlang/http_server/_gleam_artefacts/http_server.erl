-module(http_server).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([send_response/2, main_svc/1, main/0]).

-spec send_response(gleam@bytes_builder:bytes_builder(), binary()) -> gleam@http@response:response(gleam@bytes_builder:bytes_builder()).
send_response(Data, T) ->
    _pipe = gleam@http@response:new(200),
    _pipe@1 = gleam@http@response:set_header(_pipe, <<"content-type"/utf8>>, T),
    gleam@http@response:set_body(_pipe@1, Data).

-spec main_svc(gleam@http@request:request(any())) -> gleam@http@response:response(gleam@bytes_builder:bytes_builder()).
main_svc(_) ->
    _pipe = {person, <<"Jack"/utf8>>, 29, [<<"UK"/utf8>>, <<"Cardiff"/utf8>>]},
    _pipe@1 = person:construct_person_to_json(_pipe),
    send_response(_pipe@1, <<"application/json"/utf8>>).

-spec main() -> {ok, nil} | {error, gleam@dynamic:dynamic_()}.
main() ->
    gleam@http@elli:become(fun main_svc/1, 3000).
