-module(hexdump).

% this is a hack for noobes like me ...
-compile(export_all).

% here are several ways to turn a byte or better a binary to it's hex representation

% V1

bin_to_hexstr(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B:", [X]) || 
                      X <- binary_to_list(Bin)]).

% V2 - pattern matching auf signaturen

hex2(Val) when Val < 16 ->
    "0" ++ hex_(Val);

hex2(Val) ->
    hex_(Val).
 
hex_(Val) ->
    integer_to_list(Val, 16).

% V3 - pattern matching per case in der funktion
% Test 0, 1, 15, 16, 128, 255, 256

hex(Val) ->
    T = integer_to_list(Val, 16),
    case Val < 16 of
        true -> "0" ++ T;
        false -> T
    end.
             
test()->
    Input = [0, 1, 15, 16, 128, 255, 256],
    lists:foreach(fun(N) -> io:format("~s~n", [hex(N)]) end , Input),
        lists:foreach(fun(N) -> io:format("~s~n", [hex2(N)]) end , Input).

dump(Bin) when byte_size(Bin) == 0 -> "";
dump(Bin) -> dump_(binary_to_list(Bin), []).

dump_([], Acc) ->
    Acc;
dump_([Head|Tail], Acc) ->
    case length(Acc) > 0 of
        true -> dump_(Tail, Acc ++ ":" ++ hex(Head));
        false -> dump_(Tail, Acc ++ hex(Head))
    end.



