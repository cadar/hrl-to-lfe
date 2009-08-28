-module(h2l).
-export([pipe/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Program to convert nitrogen hrl file to lfe.
%%
%%   usage: cat wf.inc | erl -noshell -s h2l pipe 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pipe() -> pipe([]).

pipe(Res) ->
    case io:get_chars('', 8192) of
	eof -> process(Res);
	Text -> pipe([Res|Text])
    end.

process(Data) ->
    Name = generate_filename(),
    save(Name,Data),
    {ok, AST} = epp:parse_file(Name,[],[]),
    print(AST),
    file:delete(Name),
    init:stop().

generate_filename() ->
    {A,B,C}=now(),  N=node(),
    "h2l" ++ lists:flatten(io_lib:format("~p~p~p~p",[N,A,B,C])).

save(Name, Data) ->
    {ok,F} = file:open(Name, [read, write]),
    io:fwrite(F,"~s~n",[Data]),
    file:close(F).

print(AST) -> [lfe(C) || C<-AST].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  ast to lfe 
%%

lfe({attribute, _Line, file, _RecordName}) ->
    io:format(";; Generated with git://github.com/cadar/hrl-to-lfe.git~n~n");

lfe({attribute, _Line, record, RecordData}) ->
    {Name, Recs} = RecordData,
    io:format("(defrecord ~p",[Name]),
    [rec(C) || C<-Recs],
    io:format(")~n");

lfe({eof, _}) ->
    {{Y,M,D},{H,MM,S}}=erlang:universaltime(),
    io:format("~n;; done -"),
    io:format(" ~B-~2.10.0B-~2.10.0B",[Y,M,D]),
    io:format(" ~2.10.0B:~2.10.0B:~2.10.0B~n",[H,MM,S]);

lfe(All) ->     
    error_logger:error_msg("Untransformed record: ~p~n",[All]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  record type 
%%

rec({record_field, _, AtomData}) ->
    first_field(AtomData);

rec({record_field, _, AtomData1, AtomData2}) ->
    io:format(" ("),
    first_field(AtomData1),
    field(AtomData2),
    io:format(")");

rec({record_field, _, AtomData1, AtomData2, AtomData3}) ->
    io:format(" ("),
    first_field(AtomData1),
    field(AtomData2),
    field(AtomData3),
    io:format(")");

rec(All) ->
    error_logger:error_msg("Untransformed rec option: ~p~n",[All]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  print field type
%%

first_field({atom, _, FieldName}) -> io:format(" ~p",[FieldName]).

field({atom, _, FieldName}) -> io:format(" '~p",[FieldName]);
field({string, _, String}) -> io:format(" '\"~s\"",[String]);
field({integer, _, NR}) -> io:format(" ~p",[NR]);
field({cons, _, First, _Rest }) -> field(First);
field({nil, _ }) -> io:format(" '()");

field({tuple, _, List }) ->
    io:format(" '("),
    first_field(hd(List)),
    [field(C) || C<-tl(List)],
    io:format(")");

field(All) ->
    error_logger:error_msg("Untransformed field: ~p~n",[All]).

 
