-module(env).
-compile(export_all).
-include("types.hrl").


-spec new()-> envType().
new() ->
    dict:new().

-spec add(envType(),atom(),valType())-> envType().
add(Env,Key,Value) ->
	case dict:is_key(Key, Env) of
		true -> 
			io:format("~w is key!~n",[Key]),
			dict:update(Key, Value, Env);
		false -> 
			io:format("~w is not key!~n", [Key]),
			dict:append(Key, Value, Env)
	end.

-spec lookup(envType(),atom())-> valType().
lookup(Env,Key) -> 
   case dict:is_key(Key, Env) of
      true -> 
		{ok, [V]} = dict:find(Key, Env),
		V;
	  false -> {bool, false}
   end.

