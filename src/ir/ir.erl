-module(ir).
-compile(export_all).

stripComment(Line) ->
    case string:str(Line, "#") > 0 of
        true -> string:substr(Line, 1, string:str(Line, "#") - 1);
        false -> Line
    end.

fullstrip(Line) ->
    re:replace(Line, "\\s+", "", [global,{return,list}]).

lineToList(Line) ->
    assignmentToList(fullstrip(stripComment(Line))).

assignmentToList(Assign) ->
    string:tokens(Assign, "=").

parse(Filename) ->
    parseLines(readLines(Filename)).

parseLines(Lines) ->
    Lx = lists:map(fun(L) -> lineToList(L) end, Lines),
    % This filters out the empty Lists in the List caused by empty Lines
    % this is a hack, which should be fixed ASAP. 
    % Empty lines should not be placed in the outer list in the first place
    lists:filter(fun(X) -> length(X) /= 0 end, Lx).

readLines(Filename) ->
    {ok, File} = file:open(Filename, read),
    line_reader(File).

line_reader(File) ->
    case file:read_line(File) of
        {ok, Data} -> [Data | line_reader(File)];
        eof        -> []
    end.

