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
            split_pkt(Socket, Data),
            loop(Socket);
        {error, closed} ->
            ok
    end.

display_pkt(Data) ->
    io:format("~nDATA(~w): ", [byte_size(Data)]),
    io:format(hexdump:dump(Data)),
    io:format("~nASCII: ~s~n", [binary:part(Data, {0,4})]), 
    io:format("~n").

split_pkt(Socket, Data) ->

    % There are at least three ways of splitting the packet
    % I choose the bitstream syntax, the others are:
    %  a) binary:part
    %  b) { Hdr, Tail } = split_binary(Data, 4 ),
    % big endian =:= network byte order

    << Hdr:4/big-binary-unit:8,
       Len:1/big-integer-unit:32,
       Tail/binary >> = Data,

    io:format("~nLength: ~B~n", [Len]),

    handler(hdr_to_atom(Hdr), Socket, Tail).

handler(helo, Socket, Tail) ->
    io:format("Receiving a HELO command~n"),
    gen_tcp:send(Socket, "Thanks for your HELO.\n");

handler(stat, Socket, Tail) ->
    io:format("Recieving a STAT command~n"),
    gen_tcp:send(Socket, "Thanks for your STAT command\n");

handler(ir, Socket, Tail) ->
    io:format("Recieving a IR command~n"),
    gen_tcp:send(Socket, "Thanks for your IR command\n");

handler(undef, Socket, Tail) ->
    io:format("Could not parse command~n"),
    gen_tcp:send(Socket, "Unkown command\n").


hdr_to_atom(Header) ->
    Hdr = #{
      "HELO" => helo,
      "BYE!" => undef,
      "STAT" => stat,
      "RESP" => undef,
      "BODY" => undef,
      "META" => undef,
      "DSCO" => undef,
      "DBUG" => undef,
      "IR  " => ir,
      "RAWI" => undef,
      "ANIC" => undef,
      "BUTN" => undef,
      "KNOB" => undef,
      "SETD" => undef,
      "UREQ" => undef 
      },
    
    Prefix = binary_to_list(Header),
  
    maps:get(Prefix, Hdr).
