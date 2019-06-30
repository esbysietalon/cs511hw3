-module(interp).
%-export([scanAndParse/1,runFile/1,runStr/1]).
-compile(export_all).
-include("types.hrl").

loop(InFile,Acc) ->
    case io:request(InFile,{get_until,prompt,lexer,token,[1]}) of
        {ok,Token,_EndLine} ->
            loop(InFile,Acc ++ [Token]);
        {error,token} ->
            exit(scanning_error);
        {eof,_} ->
            Acc
    end.

scanAndParse(FileName) ->
    {ok, InFile} = file:open(FileName, [read]),
    Acc = loop(InFile,[]),
    file:close(InFile),
    {Result, AST} = parser:parse(Acc),
    case Result of 
	ok -> AST;
	_ -> io:format("Parse error~n")
    end.


-spec runFile(string()) -> valType().
runFile(FileName) ->
    valueOf(scanAndParse(FileName),env:new()).

scanAndParseString(String) ->
    {_ResultL, TKs, _L} = lexer:string(String),
    parser:parse(TKs).

-spec runStr(string()) -> valType().
runStr(String) ->
    {Result, AST} = scanAndParseString(String),
    case Result  of 
    	ok -> valueOf(AST,env:new());
    	_ -> io:format("Parse error~n")
    end.


-spec numVal2Num(numValType()) -> integer().
numVal2Num({num, N}) ->
    N.

-spec boolVal2Bool(boolValType()) -> boolean().
boolVal2Bool({bool, B}) ->
    B.

-spec valueOf(expType(),envType()) -> valType().
valueOf(Exp,Env) ->
	io:format("~w~n", [Exp]),
	case atomize(Exp, Env) of
		{packaged, O, D} ->
			OUTPUT = O,
			Dict = D;
		_ -> 
			OUTPUT = atomize(Exp, Env),
			Dict = env:new()
	end,
	case is_number(OUTPUT) of
		true -> {num, OUTPUT};
		false -> 
			case is_boolean(OUTPUT) of
				true -> {bool, OUTPUT};
				false -> 
					case isProc(OUTPUT) of 
						true -> getProc(OUTPUT, Dict);
						false -> OUTPUT
					end
			end
	end.
	%% complete
isProc(Exp) ->
	case Exp of 
		{procExp, {id, _, V}, _} -> true;
		_ -> false
	end.
getProc({procExp, {id, _, V}, FUNCEXP}, Env) ->
		{proc, V, FUNCEXP, Env}.
atomize(Exp, Env) ->
	case Exp of
		{packaged, GenExp, Env} ->
			io:format("unpackaging~n", []),
			atomize(GenExp, Env);
		{letExp, {id, _N0, V0}, VarVal, InArgs} ->
			Env0 = env:add(Env, V0, atomize(VarVal, Env)),
			atomize(InArgs, Env0);
		{procExp, IDEXP, FUNCEXP} ->
			%proc already stores this well
			{packaged, Exp, Env};
		{appExp, FUNCID, INPUT} ->
			atomize(runFunc(atomize(FUNCID, Env), atomize(INPUT, Env)), Env);
		{idExp, VarExp} -> 
			%io:format("lookup ~w~n", [VarExp]),
			atomize(termVal(VarExp, Env), Env);
		{plusExp, ADDEND1, ADDEND2} -> atomize(ADDEND1, Env)+atomize(ADDEND2, Env);
		{diffExp, ADDEND1, ADDEND2} -> atomize(ADDEND1, Env)-atomize(ADDEND2, Env);
		{numExp, _} -> termVal(Exp);
		_ ->
			%io:format("unknown_expression! ~w~n", [Exp]),
			Exp
	end.
termVal({id, _N, V}, Env) ->
	case env:lookup(Env, V) of
		{bool, false} -> undefined_variable;
		_ -> 
			%io:format("termval lookup ~w~n", [env:lookup(Env, V)]),
			env:lookup(Env, V)
	end.
termVal({E, {num, _, V}}) ->
	case E of 
		numExp -> numVal2Num({num, V});
		_ -> error
	end.
runFunc(FUNC, ARG) ->
	
	ok.