-module(log).

-export([debug/1, info/1, warn/1, error/1, 
         debug/2, info/2, warn/2, error/2]).

% error_logger:warning_msg, error_msg, info_msg
-define(PRINTER, io:format).

% for now, debug + info go to the same channel

debug(Format) ->
    ?PRINTER(Format).

debug(Format, Data) ->
    ?PRINTER(Format, Data).

info(Format) ->
    ?PRINTER(Format).

info(Format, Data) ->
    ?PRINTER(Format, Data).

warn(Format) ->
    ?PRINTER(Format).

warn(Format, Data) ->
    ?PRINTER(Format, Data).

error(Format) ->
    ?PRINTER(Format).

error(Format, Data) ->
    ?PRINTER(Format, Data).
