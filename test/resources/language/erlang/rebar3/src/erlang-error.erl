%% Test an error from Erlang
-module('erlang-error').
-compile(export_all).

great_func() ->
    io:format("Flycheck is great!");
error_func() ->
    'head-mismatch'.
