-module(squeezeclient).
% -export ([client/0, getDeviceId/1]).
-compile(export_all).

-define ( SQEEZE_PORT, 3483 ).
-define ( DESTINATION, "localhost" ).

client() ->
    Destination = "localhost",

    % This stream of bytes represents a valid client connect 
    % from a softsqueeze player starting with HELO

    % Payload = << 16#48, 16#45, 16#4c, 16#4f, 16#00, 16#00, 16#00, 
    %             16#0a, 16#06, 16#02, 16#4a, 16#bb, 16#d2, 16#68, 
    %             16#a9, 16#cf, 16#80, 16#00 >>,

    % We can use the hex-stream from wireshark if we give the
    % Size in bits (here 144)
    Payload = << 16#48454c4f0000000a06024abbd268a9cf8000:144 >>,
    
     % to make it runnable on one machine
    {ok, Sock} = gen_tcp:connect(Destination, ?SQEEZE_PORT, 
                                 [binary, {packet, 0}]),
    ok = gen_tcp:send(Sock, Payload),
    ok = gen_tcp:close(Sock).

% ----------------- HELO -----------------------

wrong_player() ->
    send_payload(<< 16#48454c4f0000000aFF024abbd268a9cf8000:144 >>).

good_player() ->
    send_payload(<< 16#48454c4f0000000a06024abbd268a9cf8000:144 >>).

% ----------------- STAT -----------------------

stat() ->
    send_payload(<< "STAT", 16#00000004:32,  16#aa55bbff:32>>).

short_stat() ->
    send_payload(<< "STAT" >>).

empty_stat() ->
    send_payload(<< "STAT", 16#00000000:32 >>).


send_payload(Payload) ->
    {ok, Sock} = gen_tcp:connect(?DESTINATION, ?SQEEZE_PORT, 
                                 [binary, {packet, 0}]),
    ok = gen_tcp:send(Sock, Payload),
    ok = gen_tcp:close(Sock).
    
getDeviceId(Id) ->
    DeviceIds = #{
      squeezebox => 2, 
      softsqueeze => 3,
      squeezebox2 => 4,
      transporter => 5,
      softsqueeze3 => 6,
      receiver => 7,
      squeezeslave => 8,
      controller => 9
      },
    
    maps:get(Id, DeviceIds).

