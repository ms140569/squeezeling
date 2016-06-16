-module(squeezeling).

-export([start/0, loop/1]).

-define ( SQUEEZE_PORT, 3483 ).

% Run Softsqueze like this:
% java -Dslimserver=localhost -jar SoftSqueeze.jar

% Run this proggy as:
% c("squeezeling"), squeezeling:start().

start() ->
    io:format("Squeezeling v0.1 running on port ~w~n", [?SQUEEZE_PORT]),
	socket_server:start(?MODULE, ?SQUEEZE_PORT, {?MODULE, loop}).
loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("~nDATA(~w): ", [byte_size(Data)]),
            io:format(bin_to_hex:bin_to_hex(Data)),
            io:format("~n"),
            gen_tcp:send(Socket, "Hi stranger\n"),
            loop(Socket);
        {error, closed} ->
            ok
    end.
