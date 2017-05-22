%% Warning from Erlang
-module('erlang-warning').
-compile(export_all).

simple_warning() ->
    io:format("Flycheck is great!", ["unused argument"]).
