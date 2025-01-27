%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc coveralls plugin for rebar3
%%% @end
%%% @author Yury Gargay <yury.gargay@gmail.com>
%%% @copyright 2013-2015 (c) Markus Ekholm <markus@botten.org>
%%% @license Copyright (c) 2013-2015, Markus Ekholm
%%% All rights reserved.
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of the <organization> nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL MARKUS EKHOLM BE LIABLE FOR ANY
%%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(rebar3_coveralls).
-behaviour(provider).

-export([ init/1
        , do/1
        , format_error/1
        ]).

-define(PROVIDER, send).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([ {name,       ?PROVIDER}
                              , {module,     ?MODULE}
                              , {namespace,  coveralls}
                              , {bare,       true}
                              , {deps,       ?DEPS}
                              , {example,    "rebar3 coveralls send"}
                              , {short_desc, "Send coverdata to coveralls."}
                              , {desc,       "Send coveralls to coveralls."}
                              , {opts,       []}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  rebar_api:info("Running coveralls...", []),
  ConvertAndSend = fun coveralls:convert_and_send_file/4,
  Get            = fun(Key, Def) -> rebar_state:get(State, Key, Def) end,
  GetLocal       = fun(Key, Def) -> rebar_state:get(State, Key, Def) end,
  MaybeSkip      = fun() -> ok end,
  ok = cover_paths(State),
  try
    rebar_coveralls:do_coveralls(ConvertAndSend,
                                 Get,
                                 GetLocal,
                                 MaybeSkip,
                                 'send-coveralls'),
    {ok, State}
  catch throw:{error, {ErrCode, Msg}} ->
      io:format("Failed sending coverdata to coveralls, ~p: ~p",
                [ErrCode, Msg]),
      {error, rebar_abort}
  end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

cover_paths(State) ->
  lists:foreach(fun(App) ->
                    AppDir = rebar_app_info:out_dir(App),
                    true   = code:add_patha(filename:join([AppDir, "ebin"])),
                    _      = code:add_patha(filename:join([AppDir, "test"]))
                end,
                rebar_state:project_apps(State)),
  _ = code:add_patha(filename:join([rebar_dir:base_dir(State), "test"])),
  ok.

%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
