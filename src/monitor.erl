-module(monitor).

% Jonas Devlieghere
% Student 0256709

-export([start/0, init/0, monitorloop/1]).

start() ->
	process_flag(trap_exit, true),
	Pid = spawn_link(monitor, init, []),
	register(monitor, Pid).

init() ->
	tile:start(16),
	monitorloop(maps:new()).

monitorloop(Map) ->
	receive
		{status, Pid, Id, Value, Merged} ->
			debug:debug("Status received from ~p ~p: (~p,~p).~n",[Id, Pid, Value, Merged]),
			NewMap = maps:put(Pid, {Id, Value, Merged}, Map),
			monitorloop(NewMap);
		{'EXIT', Pid, _} ->
			{Id, Value, Merged} = maps:get(Pid, Map),
			debug:debug("Restarting ~p with (~p,~p).~n",[Id, Value, Merged]),
			tile:tilemain(Id, Value, Merged),
			NewMap = maps:remove(Pid, Map),
			monitorloop(NewMap)
	end,
	monitorloop(Map).