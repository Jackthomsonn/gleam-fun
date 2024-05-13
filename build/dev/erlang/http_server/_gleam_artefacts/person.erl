-module(person).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([construct_person_to_json/1]).
-export_type([person/0]).

-type person() :: {person, binary(), integer(), list(binary())}.

-spec construct_person_to_json(person()) -> gleam@bytes_builder:bytes_builder().
construct_person_to_json(Person) ->
    _pipe@1 = gleam@json:object(
        [{<<"name"/utf8>>, gleam@json:string(erlang:element(2, Person))},
            {<<"age"/utf8>>, gleam@json:int(erlang:element(3, Person))},
            {<<"location"/utf8>>,
                begin
                    _pipe = erlang:element(4, Person),
                    gleam@json:array(_pipe, fun gleam@json:string/1)
                end}]
    ),
    _pipe@2 = gleam@json:to_string(_pipe@1),
    gleam_stdlib:wrap_list(_pipe@2).
