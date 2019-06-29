-module(env).
-compile(export_all).
-include("types.hrl").


-spec new()-> envType().
new() ->
    dict:new().

-spec add(envType(),atom(),valType())-> envType().
add(Env,Key,Value) ->
    dict:append(Key, Value, Env).

-spec lookup(envType(),atom())-> valType().
lookup(Env,Key) -> 
   case dict:is_key(Key, Env) of
      true -> dict:find(Key, Env);
	  false -> {bool, false}
   end.

