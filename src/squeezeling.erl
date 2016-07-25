-module(squeezeling).

%-export([start/0, loop/1]).
-compile(export_all).

-define ( SQUEEZE_PORT, 3483 ).

% Run Softsqueze like this:
% java -Dslimserver=localhost -jar SoftSqueeze.jar

% Run this proggy as:
% c("squeezeling"), squeezeling:start().

start() ->
    log:info("Squeezeling v0.1 running on port ~w~n", [?SQUEEZE_PORT]),
	socket_server:start(?MODULE, ?SQUEEZE_PORT, {?MODULE, loop}).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            display_pkt(Data),
            {Cmd, Len, Payload} = split_pkt(Data),
            log:info("~nLength of Payload in bytes : ~B~n", [Len]),
            handler(Cmd, Socket, Payload),
            loop(Socket);
        {error, closed} ->
            ok
    end.

display_pkt(Data) ->
    log:debug("~nDATA(~w): ", [byte_size(Data)]),
    log:debug(hexdump:dump(Data)),
    log:debug("~nASCII: ~s~n", [binary:part(Data, {0,4})]), 
    log:debug("~n").

split_pkt(Data) ->

    case size(Data) < 8 of
        true ->
            log:warn("~nPacket too short~n"),
            {undef, 0, 0 };
        false ->

            % There are at least three ways of splitting the packet
            % I choose the bitstream syntax, the others are:
            %  a) binary:part
            %  b) { Hdr, Tail } = split_binary(Data, 4 ),
            % big endian =:= network byte order

            << Hdr:4/big-binary-unit:8,
               Len:1/big-integer-unit:32,
               Payload/binary >> = Data,

            { hdr_to_atom(Hdr), Len, Payload }
    end.

handler(helo, Socket, Payload) ->
    log:info("Receiving a HELO command~n"),

    << DeviceID:1/big-integer-unit:8,
       Revision:1/big-integer-unit:8,
       Mac:6/big-binary-unit:8,
       ChannelList:2/big-integer-unit:8 >> = Payload,

    Device = getDeviceForNumber(DeviceID),

    case Device of
        undef -> 
            log:warn("~nDevice not found : ~w~n", [Device]),
            gen_tcp:send(Socket, "I do not know you.\n"),
            gen_tcp:close();
        _ ->
            log:debug("~nDevice   : ~w", [Device]),
            log:debug("~nRevision : ~p", [Revision]),
            log:debug("~nMAC      : " ++ hexdump:dump(Mac)),
            log:debug("~nChannels : ~p", [ChannelList]),
            log:debug("~n"),
            gen_tcp:send(Socket, "Thanks for your HELO.\n")
    end;

handler(stat, Socket, Payload) ->
    log:debug("~nRecieving a STAT command : " ++ hexdump:dump(Payload) ++ "~n"),
    gen_tcp:send(Socket, "Thanks for your STAT command\n");

handler(butn, Socket, Payload) ->
    log:debug("~nRecieving a BUTN command : " ++ hexdump:dump(Payload) ++ "~n"),

    << Time:4/big-integer-unit:8,
       Button:4/big-binary-unit:8 >> = Payload,
    
    log:debug("~nTime   : ~p", [Time]),
    log:debug("~nButton : " ++ hexdump:dump(Button)),
    log:debug("~n"),

    gen_tcp:send(Socket, "Thanks for your BUTN command\n");

handler(ir, Socket, Payload) ->
    log:debug("~nRecieving a IR command : " ++ hexdump:dump(Payload) ++ "~n"),

    << Time:4/big-integer-unit:8,
       Format:1/big-binary-unit:8,
       NoBits:1/big-binary-unit:8,
       IRCode:4/big-binary-unit:8 >> = Payload,
    
    log:debug("~nTime   : ~p", [Time]),
    log:debug("~nFormat : " ++ hexdump:dump(Format)),
    log:debug("~nNoBits : " ++ hexdump:dump(NoBits)),
    log:debug("~nIRCode : " ++ hexdump:dump(IRCode)),
    log:debug("~n"),

    gen_tcp:send(Socket, "Thanks for your IR command\n");

handler(undef, Socket, Payload) ->
    log:warn("~nCould not parse command : " ++ hexdump:dump(Payload) ++ "~n"),
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
      "BUTN" => butn,
      "KNOB" => undef,
      "SETD" => undef,
      "UREQ" => undef 
      },
    
    Prefix = binary_to_list(Header),
  
    maps:get(Prefix, Hdr).

getDeviceForNumber(Id) ->
    DeviceIds = #{
       2 => squeezebox, 
       3 => softsqueeze,
       4 => squeezebox2,
       5 => transporter,
       6 => softsqueeze3,
       7 => receiver,
       8 => squeezeslave,
       9 => controller,
      10 => boom,
      11 => softboom,
      12 => squeezeplay
      },
    % return undef in case this device-number is
    % not registered.
    maps:get(Id, DeviceIds, undef).

