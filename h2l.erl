-module(h2l).
-export([pipe/0]).

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
    print_tran(AST),
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

print_tran(AST) ->
    io:format("~p~n",[AST]).


print(AST) -> [lfe(C) || C<-AST].

lfe({attribute, _Line, file, _RecordName}) ->
    io:format(";; 1. git clone git://github.com/cadar/hrl-to-lfe.git ; cd hrl-to-lfe~n"),
    io:format(";; 2. erlc h2l.erl ; cat wf.inc | erl -noshell -s h2l pipe > wf.lfe~n~n");

lfe({attribute, _Line, record, RecordData}) ->
    {Name, Recs} = RecordData,
    io:format("(defrecord ~p",[Name]),
    [rec(C) || C<-Recs],
    io:format(")~n~n");

lfe({eof, _}) ->
    io:format("~n;; done~n");

lfe(All) ->     
    error_logger:error_msg("Untransformed record: ~p~n",[All]).


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

first_field({atom, _, FieldName}) ->
    io:format(" ~p",[FieldName]).

field({atom, _, FieldName}) ->
    io:format(" '~p",[FieldName]);

field({string, _, FieldName}) ->
    io:format(" '~s",[to_str(FieldName)]);

field({cons, _, First, Rest }) ->
    field(First);
field({tuple, _, List }) ->
    io:format(" '("),
    first_field(hd(List)),
    [field(C) || C<-tl(List)],
    io:format(")");

field({nil, _ }) -> 0;

field(All) ->
    error_logger:error_msg("Untransformed field: ~p~n",[All]).


to_str([]) ->
     '""';
to_str(All) ->
    All.


to_sexp([]) ->
     '()';
to_sexp(All) ->
    All.

% (defrecord elementbase (module 'undefined)      id actions (show_if 'true) (class '"") (style '""))
% (defrecord link (module 'element_link)          id actions (show_if 'true) (class '"") (style '"") (text '"") (body '"") (html_encode 'true) (url '"javascript:") postback)
% (defrecord datepicker_textbox  (module 'element_datepicker_textbox)          id actions (show_if 'true) (class '"") (style '"") (text '"") next (html_encode 'true) (validators '()) (options '"{dateFormat:'yy-mm-dd'}"))

 
