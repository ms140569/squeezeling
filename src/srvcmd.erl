-module(srvcmd).
% -export ([client/0, getDeviceId/1]).
-compile(export_all).

-define ( SQEEZE_PORT, 3483 ).
-define ( DESTINATION, "localhost" ).

cmd(Cmd, Payload) ->
    % size - 2 bytes, command header eg. "strm" - 4 bytes, payload
    Size = size(Payload) + 4,
    StrCmd = list_to_binary(command(Cmd)),

    << Size:1/big-integer-unit:16, 
       StrCmd/binary, 
       Payload/binary >>.

command(Cmd) ->
    case Cmd of
        strm -> "strm";
        vers -> "vers";
        _  -> undef
    end.
