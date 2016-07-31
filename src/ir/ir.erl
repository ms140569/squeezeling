-module(ir).
-compile(export_all).

parse(Filename) ->
    {ok, File} = file:open(Filename, read),
    line_reader(File).

line_reader(File) ->
    case file:read_line(File) of
        {ok, Data} -> [Data | line_reader(File)];
        eof        -> []
    end.

