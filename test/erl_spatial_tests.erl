-module(erl_spatial_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erl_spatial.hrl").

-define(COUNT, 30000).
-define(BOUND, 6000000).

index_create_test() ->
	% test create an in-memory r-tree
	?assertEqual({ok, <<>>}, erl_spatial:index_create()).

index_test() ->
	Pt = "{\"type\":\"Point\",\"coordinates\":[0.5, 0.5]}",
	Poly = "{ \"type\": \"Polygon\",
  		\"coordinates\": [
    		[ [0.0, 0.0], [1.0, 0.0], [1.0, 1.0], [0.0, 1.0], [0.0, 0.0] ]
    	]
 	}",
 	Multi = "{\"type\": \"GeometryCollection\",
				\"geometries\": [
		    {\"type\": \"Point\",
		      \"coordinates\": [0.5, 0.5 ]
		      },
		    { \"type\": \"LineString\",
		      \"coordinates\": [ [0.0, 0.0], [1.0, 1.0] ]
		    }
		  ]
	}",

	{ok, Idx} = erl_spatial:index_create(),	
	?assertEqual(ok, erl_spatial:index_insert(Idx, <<"point">>, Pt)),

	?assertEqual({ok, [{0.5, 0.5}, {0.5, 0.5}]}, erl_spatial:index_bounds(Idx)),
	
	% test insertion of geometry types to check MBR code
	?assertEqual(ok, erl_spatial:index_insert(Idx, <<"poly">>, Poly)),
	?assertEqual(ok, erl_spatial:index_insert(Idx, <<"multi">>, Multi)),
	?assertEqual({ok, [{0.0, 0.0}, {1.0, 1.0}]}, erl_spatial:index_bounds(Idx)),

 	% check we hit and then check the intersection for the original
 	?assertEqual({ok, 3}, erl_spatial:index_intersects_count(Idx, 
 													{0.5, 0.5}, {0.5, 0.5})),
  	?assertEqual({ok, [<<"multi">>, <<"poly">>]}, erl_spatial:index_intersects(Idx, 
  													{0.0, 1.0}, {0.0, 1.0})),

	% check the misses
	?assertEqual({ok, 0}, 
		erl_spatial:index_intersects_count(Idx, {2, 2}, {3, 3})),
	
	% clean up
	?assertEqual(ok, erl_spatial:index_delete(Idx, <<"point">>, Pt)),
	?assertEqual(ok, erl_spatial:index_delete(Idx, <<"poly">>, Poly)),
	?assertEqual(ok, erl_spatial:index_delete(Idx, <<"multi">>, Multi)),
	?assertEqual({ok, 0}, erl_spatial:index_intersects_count(Idx, 
															{0, 0}, {1, 1})).

% benchmarks - tests to make sure everything is going faster enough
%  The python benchmarks don't use wkb_writer or geos
% loosely copied from the python r-tree wrapper on libspatialindex
% hobu's latest results on his 2006-era machine

% Stream load:
% 293710.04 usec/pass
 
% One-at-a-time load:
% 527883.95 usec/pass
 
% 30000 points
% Query box:  (1240000, 1010000, 1400000, 1390000)

% Brute Force:
% 46 hits
% 13533.60 usec/pass
 
% Memory-based Rtree Intersection:
% 46 hits
% 7516.19 usec/pass
 
% Disk-based Rtree Intersection:
% 46 hits
% 7543.00 usec/pass

% Disk-based Rtree Intersection without Item() wrapper (objects='raw'):
% 46 raw hits
% 347.60 usec/pass

% insert_test() ->
% 	% insert is only done on 2000 pts (seee pyRtree benchmarks.py)
% 	Coords = create_test_list([], 0, 2000),
% 	{ok, Idx} = erl_spatial:index_create(),
	
% 	T1 = erlang:now(),
% 	lists:foreach(fun({Id, JSON}) ->
% 			erl_spatial:index_insert(Idx, Id, JSON)
% 		end, Coords
% 	),
% 	T2 = erlang:now(),
% 	Diff = timer:now_diff(T2, T1),
% 	% one at a time load value on MBA 
% 	% python ctypes result 395900.50 
% 	% erlang result ~ 107102
% 	?debugFmt("~nOne-at-a-time load: ~p usec~n", [Diff]),
% 	?assert(Diff =< 527883.95).


% intersect_test_() ->
% 	{timeout, 60,
% 	fun() ->
% 		Coords = create_test_list([], 0, ?COUNT),
% 		{ok, Idx} = erl_spatial:index_create(),
% 		{ok, DiskIdx} = erl_spatial:index_create([{?IDX_FILENAME, 
% 													"/tmp/test"}]),
% 		lists:foreach(fun({Id, JSON}) ->
% 				erl_spatial:index_insert(Idx, Id, JSON),
% 				erl_spatial:index_insert(DiskIdx, Id, JSON)
% 			end, Coords
% 		),

% 		MinX = 1240000,
% 		MinY = 1010000,
% 		MaxX = 1400000,
% 		MaxY = 1390000,

% 		% memory based index
% 		T3 = erlang:now(),
% 		{ok, MemHits} = erl_spatial:index_intersects(Idx, {MinX, MinY}, 
% 														{MaxX, MaxY}),
% 		T4 = erlang:now(),
% 		D2 = timer:now_diff(T4, T3),
% 		% memory based intersection on MBA
% 		% python ctypes result 4929.33 usec
% 		% erlang result ~ 178 usec
% 		?debugFmt("~nMemory based intersection: hits ~p, ~p usec~n",
% 			[length(MemHits), D2]),
% 		?assert(D2 =< 7516.19),

% 		% run same test on a disk index
% 		T5 = erlang:now(),
% 		{ok, DiskHits} = erl_spatial:index_intersects(DiskIdx, {MinX, MinY}, 
% 														{MaxX, MaxY}),
% 		T6 = erlang:now(),
% 		D3 = timer:now_diff(T6, T5),
% 		% disk based intersection on MBA with item wrapper (required for couch)
% 		% python ctypes result 4653.50 usec
% 		% erlang result  ~ 409 usec
% 		?debugFmt("~nDisk based intersection: hits ~p, ~p usec ~n",
% 			[length(DiskHits), D3]),
% 		?assert(D3 =< 4653.50),
% 		% cleanup
% 		?assertEqual(ok, file:delete("/tmp/test.dat")),
% 		?assertEqual(ok, file:delete("/tmp/test.idx"))
% 	end}.

% create_test_list(Acc, Cntr, Max) when Cntr == Max ->
% 	Acc;

% create_test_list(Acc, Cntr, Max) ->	
% 	X = random:uniform(?BOUND) * 1.0,
% 	Y = random:uniform(?BOUND) * 1.0,
%     JSON = io_lib:format("{\"type\":\"Point\",\"coordinates\":[~f, ~f]}",
%     	[X,Y]),
% 	Pt = {list_to_binary(integer_to_list(Cntr)), JSON},
% 	create_test_list([Pt |Acc],
% 		 Cntr + 1, Max).