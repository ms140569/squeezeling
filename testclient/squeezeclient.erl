-module(squeezeclient).
-export ([client/0]).

-define ( SQEEZE_PORT, 3483 ).

client() ->
    Destination = "localhost",
     % to make it runnable on one machine
    {ok, Sock} = gen_tcp:connect(Destination, ?SQEEZE_PORT, 
                                 [binary, {packet, 0}]),
    ok = gen_tcp:send(Sock, "HELO"),
    ok = gen_tcp:close(Sock).

