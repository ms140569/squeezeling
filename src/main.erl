-module(main).
-export ([start/2, start_servers/2]).

%server() ->
%    {ok, LSock} = gen_tcp:listen(5678, [binary, {packet, 0}, 
%                                        {active, false}]),
%    {ok, Sock} = gen_tcp:accept(LSock),
%    {ok, Bin} = do_recv(Sock, []),
%    ok = gen_tcp:close(Sock),
%    Bin.

%do_recv(Sock, Bs) ->
%    case gen_tcp:recv(Sock, 0) of
%       {ok, B} ->
%            do_recv(Sock, [Bs, B]);
%        {error, closed} ->
%            {ok, list_to_binary(Bs)}
%    end.

% ---------------

start(Num,LPort) ->
    
    case gen_tcp:listen(LPort,[{active, false},{packet,2}]) of
        {ok, ListenSock} ->
            
            start_servers(Num,ListenSock),
            {ok, Port} = inet:port(ListenSock),
            Port;
                {error,Reason} ->
            {error,Reason}
    end.

start_servers(0,_) ->
    ok;
start_servers(Num,LS) ->
    spawn(?MODULE,server,[LS]),
    start_servers(Num-1,LS).

server(LS) ->
    case gen_tcp:accept(LS) of
        {ok,S} ->
            loop(S),
            server(LS);
        Other ->
            io:format("accept returned ~w - goodbye!~n",[Other]),
            ok
    end.

loop(S) ->
    inet:setopts(S,[{active,once}]),
    receive
        {tcp,S,Data} ->
            Answer = process(Data), % Not implemented in this example
            gen_tcp:send(S,Answer),
            loop(S);
        {tcp_closed,S} ->
            io:format("Socket ~w closed [~w]~n",[S,self()]),
            ok
    end.

process(_) ->
    "nix".

