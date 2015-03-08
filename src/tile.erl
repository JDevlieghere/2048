-module(tile).

% Jonas Devlieghere
% Student 0256709

-export([start/1 ,tilemain/1, tilemain/2, tilemain/3, opposite/1, neighbor/2, boundary/2,
	propagate/2, tilelife/3, valid/3]).

tilemain(Id) ->
	tilemain(Id, 0, false).

tilemain(Id, Value) ->
	tilemain(Id, Value, false).

tilemain(Id, Value, Merged) ->
	process_flag(trap_exit, true),
	Pid = spawn_link(tile, tilelife, [Id, Value, Merged]),
	glob:registerName(glob:regformat(Id), Pid),
	monitor ! {status, Pid, Id, Value, Merged},
	manager ! ready,
	debug:debug("Tile ~p has been spawned with Pid ~p\n",[Id,Pid]).

% Stat All Tiles
start(0) -> ok;
start(N) ->
	tilemain(N),
	start(N - 1).

% Get the neighboring tile in the given direction.
neighbor(Id, Dir) ->
	case Dir of
		up -> Id - 4;
		dn -> Id + 4;
		lx -> Id - 1;
		rx -> Id + 1
	end.

% Get the opposite direction of the given direction.
opposite(Dir) ->
	case Dir of
		up -> dn;
		dn -> up;
		lx -> rx;
		rx -> lx
	end.

% Check whether the given tile is a boundary element in the given direction.
boundary(Id, Dir) ->
	case Dir of
		up -> (Id - 4) < 1;
		dn -> (Id + 4) >  16;
		lx -> (Id - 1) rem 4 == 0;
		rx -> Id rem 4 == 0
	end.

% Check whether it's valid for another tile to move to this tile.
valid(CurrentValue, BestValue, Merged) ->
	(CurrentValue == 0) or ((BestValue == CurrentValue) and not Merged).

% Propagate the movement in the given direction. When the element is a boundary
% element, start resetting in the opposite direction.
propagate(Id, Dir) ->
	HasNeighbor = not boundary(Id, Dir),
	case HasNeighbor of
		true ->
			glob:regformat(neighbor(Id, Dir)) ! opposite(Dir);
		false ->
			reset(Id, opposite(Dir))
	end.

% Reset the current tile and propagate in the given direction. Stop when the
% boundary has been reached.
reset(Id, Dir) ->
	HasNeighbor = not boundary(Id, Dir),
	manager ! ready,
	case HasNeighbor of
		true -> glob:regformat(neighbor(Id, Dir)) ! {reset, Dir};
		false -> ok
	end.

% Move the tile in the given direction.
% - If the value is 0, the move can be forwarded to the next tile in the
%   opposite direction.
% - If the tile can move, find the best tile to move to. Otherwise propagate
%   the move to the next tile in line.
%
move(Id, Dir, 0) ->
	debug:debug("Move tile ~p in direction ~p.~n",[Id, Dir]),
	propagate(Id, opposite(Dir));
move(Id, Dir, CurrentValue) ->
	debug:debug("Move tile ~p in direction ~p.~n",[Id, Dir]),
	HasNeighbor = not boundary(Id, Dir),
	case HasNeighbor of
		true -> glob:regformat(neighbor(Id, Dir)) ! {moveRequest, Dir, Id, CurrentValue, Id, CurrentValue};
		false -> propagate(Id, opposite(Dir))
	end.

tilelife(Id, CurrentValue, Merged)->
	receive
		die ->
			debug:debug("I, ~p, die.~n",[Id]),
			exit(killed);
		up ->
			move(Id, up, CurrentValue);
		dn ->
			move(Id, dn, CurrentValue);
		lx ->
			move(Id, lx, CurrentValue);
		rx ->
			move(Id, rx, CurrentValue);
		{moveRequest, Dir, ReqId, ReqValue, BestId, BestValue} ->
			debug:debug("Considering tile ~p (Value=~p,Merged=~p) as a new position for tile ~p (Value=~p).~n",[Id, CurrentValue, Merged, ReqId, ReqValue]),
			Boundary = boundary(Id, Dir),
			CanMoveHere = valid(CurrentValue, ReqValue, Merged),
			case CanMoveHere of
				false ->
					glob:regformat(ReqId) ! {moveReply, Dir, BestId, BestValue};
				true ->
					case Boundary of
						false ->
							glob:regformat(neighbor(Id, Dir)) ! {moveRequest, Dir, ReqId, ReqValue, Id, CurrentValue};
						true ->
							glob:regformat(ReqId) ! {moveReply, Dir, Id, CurrentValue}
					end
			end;
		{moveReply, Dir, BestId, BestValue} ->
			debug:debug("Found tile ~p (Value=~p) as a new position for tile ~p (Value=~p).~n",[BestId, BestValue, Id, CurrentValue]),
			if
				BestId == Id ->
					propagate(Id, opposite(Dir));
				BestValue == CurrentValue ->
					glob:regformat(BestId) ! {setvalue, 2*CurrentValue, true},
					propagate(Id, opposite(Dir)),
					tilelife(Id, 0, false);
				BestValue == 0 ->
					glob:regformat(BestId) ! {setvalue, CurrentValue, false},
					propagate(Id, opposite(Dir)),
					tilelife(Id, 0, false)
			end;
		{yourValue, Repl} ->
			Repl ! {tilevalue, Id, CurrentValue, Merged};
		{setvalue, Future, NewMerged} ->
			monitor ! {status, self(), Id, Future, NewMerged},
			tilelife(Id, Future, NewMerged);
		{reset, Dir} ->
			reset(Id, Dir),
			tilelife(Id, CurrentValue, false)
	end,
	tilelife(Id, CurrentValue, Merged).
