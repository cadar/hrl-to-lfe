-module(h2l).
-export([io/0, tran/1]).


io() -> io([]).

io(Res) ->
    case io:get_chars('', 8192) of
	eof -> process(Res);
	Text -> io([Res|Text])
    end.

process(Data) ->
    Name = generate_filename(),
    save(Name,Data),
    AST = epp:parse_file(Name,[],[]),
    tran(AST),
    init:stop().

generate_filename() ->
    {A,B,C}=now(),  N=node(),
    "/tmp/.h2l-" ++ lists:flatten(io_lib:format("~p-~p.~p.~p",[N,A,B,C])).

save(Name, Data) ->
    io:format("Now writing to: ~s~n",[Name]),
    {ok,F} = file:open(Name, [read, write]),
    io:fwrite(F,"~s~n",[Data]),
    file:close(F).

tran(AST) ->
    io:format("~p~n",[AST]).


