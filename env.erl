-module(env).
-compile(export_all).
-include("types.hrl").


-spec new()-> envType().
new() ->
    % complete

-spec add(envType(),atom(),valType())-> envType().
add(Env,Key,Value) ->
    % complete

-spec lookup(envType(),atom())-> valType().
lookup(Env,Key) -> 
   % complete

