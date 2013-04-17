%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2013 by Tony Rogvall <tony@rogvall.se>

-module(gsms).

-export([start/0, stop/0]).

start() ->
    call([lager,ale,uart,gsms], start).

stop() ->
    call([gsms,uart,ale,lager], stop).

call([App|Apps], F) ->
    error_logger:info_msg("~p: ~p\n", [F,App]),
    case application:F(App) of
	{error,{not_started,App1}} ->
	    call([App1,App|Apps], F);
	{error,{already_started,App}} ->
	    call(Apps, F);
	ok ->
	    call(Apps, F);
	Error ->
	    Error
    end.
