-module(gleam@http@elli).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([start/2, become/2]).
-export_type([elli_request/0, start_link_option/0]).

-type elli_request() :: any().

-type start_link_option() :: {callback, gleam@erlang@atom:atom_()} |
    {callback_args,
        fun((elli_request()) -> {integer(),
            list({binary(), binary()}),
            gleam@bytes_builder:bytes_builder()})} |
    {port, integer()}.

-spec get_method(elli_request()) -> gleam@http:method().
get_method(Req) ->
    _pipe = Req,
    _pipe@1 = elli_request:method(_pipe),
    _pipe@2 = gleam@http:method_from_dynamic(_pipe@1),
    gleam@result:unwrap(_pipe@2, get).

-spec get_port(elli_request()) -> gleam@option:option(integer()).
get_port(Req) ->
    _pipe = Req,
    _pipe@1 = elli_request:port(_pipe),
    _pipe@2 = gleam@dynamic:int(_pipe@1),
    gleam@option:from_result(_pipe@2).

-spec get_scheme(elli_request()) -> gleam@http:scheme().
get_scheme(Req) ->
    Scheme = begin
        _pipe = Req,
        _pipe@1 = elli_request:scheme(_pipe),
        _pipe@2 = gleam@dynamic:string(_pipe@1),
        _pipe@3 = gleam@result:unwrap(_pipe@2, <<""/utf8>>),
        gleam@string:lowercase(_pipe@3)
    end,
    case Scheme of
        <<"https"/utf8>> ->
            https;

        _ ->
            http
    end.

-spec get_path(elli_request()) -> binary().
get_path(Request) ->
    Raw_path = elli_request:raw_path(Request),
    _pipe = Raw_path,
    _pipe@1 = binary:split(_pipe, [<<"#"/utf8>>, <<"?"/utf8>>]),
    _pipe@2 = gleam@list:first(_pipe@1),
    gleam@result:unwrap(_pipe@2, Raw_path).

-spec convert_header_to_lowercase({binary(), binary()}) -> {binary(), binary()}.
convert_header_to_lowercase(Header) ->
    gleam@pair:map_first(Header, fun(Key) -> gleam@string:lowercase(Key) end).

-spec service_to_elli_handler(
    fun((gleam@http@request:request(bitstring())) -> gleam@http@response:response(gleam@bytes_builder:bytes_builder()))
) -> fun((elli_request()) -> {integer(),
    list({binary(), binary()}),
    gleam@bytes_builder:bytes_builder()}).
service_to_elli_handler(Service) ->
    fun(Req) ->
        Resp = begin
            _pipe@1 = {request,
                get_method(Req),
                begin
                    _pipe = elli_request:headers(Req),
                    gleam@list:map(_pipe, fun convert_header_to_lowercase/1)
                end,
                elli_request:body(Req),
                get_scheme(Req),
                gleam_elli_native:get_host(Req),
                get_port(Req),
                get_path(Req),
                {some, elli_request:query_str(Req)}},
            Service(_pipe@1)
        end,
        {response, Status, Headers, Body} = Resp,
        {Status, Headers, Body}
    end.

-spec start(
    fun((gleam@http@request:request(bitstring())) -> gleam@http@response:response(gleam@bytes_builder:bytes_builder())),
    integer()
) -> {ok, gleam@erlang@process:pid_()} | {error, gleam@dynamic:dynamic_()}.
start(Service, Number) ->
    _pipe = [{port, Number},
        {callback, erlang:binary_to_atom(<<"gleam_elli_native"/utf8>>)},
        {callback_args, service_to_elli_handler(Service)}],
    elli:start_link(_pipe).

-spec become(
    fun((gleam@http@request:request(bitstring())) -> gleam@http@response:response(gleam@bytes_builder:bytes_builder())),
    integer()
) -> {ok, nil} | {error, gleam@dynamic:dynamic_()}.
become(Service, Number) ->
    _pipe = Service,
    _pipe@1 = start(_pipe, Number),
    gleam@result:map(_pipe@1, fun gleam_elli_native:await_shutdown/1).
