-module(squeezeling).

%-export([start/0, loop/1]).
-compile(export_all).

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
            display_pkt(Data),
            handle_pkt(Socket, Data),
            loop(Socket);
        {error, closed} ->
            ok
    end.

display_pkt(Data) ->
    io:format("~nDATA(~w): ", [byte_size(Data)]),
    io:format(hexdump:dump(Data)),
    io:format("~n").

handle_pkt(Socket, Data) ->
    case hdr_parser(Data) of
        helo -> handle_helo(Socket, Data);
        undef -> handle_undef(Socket, Data)
    end.

handle_helo(Socket, Data) ->
    io:format("Receiving a HELO command~n"),
    gen_tcp:send(Socket, "Thanks for your HELO.\n").

handle_undef(Socket, Data) ->
    io:format("Could not parse command~n"),
    gen_tcp:send(Socket, "Unkown command\n").


hdr_parser(Data) ->
    Hdr = #{
      "HELO" => helo,
      "BYE!" => undef,
      "STAT" => undef,
      "RESP" => undef,
      "BODY" => undef,
      "META" => undef,
      "DSCO" => undef,
      "DBUG" => undef,
      "IR  " => undef,
      "RAWI" => undef,
      "ANIC" => undef,
      "BUTN" => undef,
      "KNOB" => undef,
      "SETD" => undef,
      "UREQ" => undef 
      },
    
    Prefix = binary_to_list(binary:part(Data, {0,4})),
  
    maps:get(Prefix, Hdr).
