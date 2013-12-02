% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.
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

 	% check we hit and then check the intersection (mbr and exact) 
 	?assertEqual({ok, 3}, erl_spatial:index_intersects_count(Idx, 
 													{0.5, 0.5}, {0.5, 0.5})),

 	?assertEqual({ok, [<<"multi">>, <<"poly">>]}, 
  			erl_spatial:index_intersects_mbr(Idx, {0.0, 1.0}, {0.0, 1.0})),
 
	?assertEqual({ok, [<<"multi">>, <<"poly">>]}, 
  			erl_spatial:index_intersects(Idx, {1.0, 0.0}, {1.0, 0.0})),
 	
	% wkt test
	?assertEqual({ok, [<<"multi">>, <<"poly">>, <<"point">>]},
		erl_spatial:index_intersects(Idx, "POLYGON ((0 1, 0 0, 1 0, 1 1, 0 1))")),
 	
	% polygon with hole
	?assertEqual({ok, [<<"multi">>, <<"poly">>]},
		erl_spatial:index_intersects(Idx, "POLYGON ((0 1, 0 0, 1 0, 1 1, 0 1),
			(0.25 0.75, 0.25 0.25, 0.75 0.25, 0.75 0.75, 0.25 0.75))")),
 
	?assertEqual({ok, [<<"poly">>]},
		erl_spatial:index_intersects(Idx, "POINT(1 0)")),

	% check the misses
	?assertEqual({ok, 0}, 
		erl_spatial:index_intersects_count(Idx, {2, 2}, {3, 3})),

	% contains
	?assertEqual({ok, [<<"point">>]}, 
		erl_spatial:index_contains(Idx, "POLYGON ((0.25 0.75, 0.25 0.25, 0.75 0.25, 0.75 0.75, 0.25 0.75))")),
	
	% contains properly
	?assertEqual({ok, [<<"point">>]}, 
		erl_spatial:index_contains_properly(Idx, "POLYGON ((0.25 0.75, 0.25 0.25, 0.75 0.25, 0.75 0.75, 0.25 0.75))")),

	% covered by
	?assertEqual({ok, [<<"poly">>]}, 
		erl_spatial:index_covered_by(Idx, "POLYGON ((0 1, 0 0, 1 0, 1 1, 0 1))")),

	% covers
	?assertEqual({ok, [<<"point">>]}, 
		erl_spatial:index_covers(Idx, "POINT(0.5 0.5)")),

	% crosses
	?assertEqual({ok, [<<"poly">>]}, 
		erl_spatial:index_crosses(Idx, "LINESTRING (0.5 0.5, 1.5 1.5)")),

	% disjoint
	?assertEqual({ok, [<<"multi">>]}, 
		erl_spatial:index_disjoint(Idx, "POINT (1 0)")),

	% overlaps
	?assertEqual({ok, [<<"poly">>]}, 
		erl_spatial:index_overlaps(Idx, "POLYGON ((0.5 1.5, 0.5 0.5, 1.5 0.5, 1.5 1.5, 0.5 1.5))")),

	% touches
	?assertEqual({ok, [<<"multi">>, <<"poly">>]}, 
		erl_spatial:index_touches(Idx, "LINESTRING (0 1, 0 0)")),

	% within
	?assertEqual({ok, [<<"multi">>, <<"poly">>, <<"point">>]},
		erl_spatial:index_within(Idx, "POINT(0.5 0.5)")),

	% clean up
	?assertEqual(ok, erl_spatial:index_delete(Idx, <<"point">>, Pt)),
	?assertEqual(ok, erl_spatial:index_delete(Idx, <<"poly">>, Poly)),
	?assertEqual(ok, erl_spatial:index_delete(Idx, <<"multi">>, Multi)),
	?assertEqual({ok, 0}, erl_spatial:index_intersects_count(Idx, 
															{0, 0}, {1, 1})),
	% test each of the other spatial functions
	% radius test at the equator 0.001 degs ~ 111 metres
	% index_intersects(_Idx, {_Lon, _Lat, _Radius, _DbCrs}) -
	Pt1 = "{\"type\":\"Point\",\"coordinates\":[0.0, 0.0]}",
	Pt2 = "{\"type\":\"Point\",\"coordinates\":[0.001, 0.0]}",
	?assertEqual(ok, erl_spatial:index_insert(Idx, <<"pt1">>,
		Pt1)),
	?assertEqual(ok, erl_spatial:index_insert(Idx, <<"pt2">>,
		Pt2)),

	% radius of 100 metres
	?assertEqual({ok, [<<"pt1">>]},
		erl_spatial:index_intersects(Idx, {0, 0, 100})),

	% radius of 120 metres
	?assertEqual({ok, [<<"pt2">>, <<"pt1">>]},
		erl_spatial:index_intersects(Idx, {0, 0, 120})),

	% test ellipse arc polygon
	?assertEqual({ok, [<<"pt1">>]},
		erl_spatial:index_intersects(Idx, {0, 0, 100, 100})),

	% real tests
	% CO ski area, lat lon
	?assertEqual(ok, erl_spatial:index_insert(Idx, <<"ski">>,
		"{\"type\":\"Point\",\"coordinates\":[-106.23779296875, 40.09067983779908]}")),

	% mutex test
	lists:foreach(fun(_X) -> 
		spawn(fun() ->
			 ?assertEqual({ok, [<<"ski">>]},
				erl_spatial:index_intersects(Idx, {-106.23779296875, 40.09067983779908, 50}))
			end)
		end, lists:seq(1, 20)),

	% radius reprojection test, 0 equals default db crs of LL
	% EPSG:26913
	% projected coordinates, 394480, 4438555
	CRS = "urn:ogc:def:crs:EPSG::26913",
	?assertEqual({ok, [<<"ski">>]},
		erl_spatial:index_intersects(Idx, {394480, 4438555, 50}, CRS, 0)),

	% reprojection test with min/max and a 50m box
	?assertEqual({ok, [<<"ski">>]}, erl_spatial:index_intersects(Idx, {394480, 4438555}, {394530, 4438605}, CRS, 0)),

	% UK
	?assertEqual(ok, erl_spatial:index_insert(Idx, <<"uk">>, "{\"type\":\"MultiPolygon\",\"coordinates\":[[[[-5.661949,54.554603],[-6.197885,53.867565],[-6.95373,54.073702],[-7.572168,54.059956],[-7.366031,54.595841],[-7.572168,55.131622],[-6.733847,55.17286],[-5.661949,54.554603]]],[[[-3.005005,58.635],[-4.073828,57.553025],[-3.055002,57.690019],[-1.959281,57.6848],[-2.219988,56.870017],[-3.119003,55.973793],[-2.085009,55.909998],[-2.005676,55.804903],[-1.114991,54.624986],[-0.430485,54.464376],[0.184981,53.325014],[0.469977,52.929999],[1.681531,52.73952],[1.559988,52.099998],[1.050562,51.806761],[1.449865,51.289428],[0.550334,50.765739],[-0.787517,50.774989],[-2.489998,50.500019],[-2.956274,50.69688],[-3.617448,50.228356],[-4.542508,50.341837],[-5.245023,49.96],[-5.776567,50.159678],[-4.30999,51.210001],[-3.414851,51.426009],[-3.422719,51.426848],[-4.984367,51.593466],[-5.267296,51.9914],[-4.222347,52.301356],[-4.770013,52.840005],[-4.579999,53.495004],[-3.093831,53.404547],[-3.09208,53.404441],[-2.945009,53.985],[-3.614701,54.600937],[-3.630005,54.615013],[-4.844169,54.790971],[-5.082527,55.061601],[-4.719112,55.508473],[-5.047981,55.783986],[-5.586398,55.311146],[-5.644999,56.275015],[-6.149981,56.78501],[-5.786825,57.818848],[-5.009999,58.630013],[-4.211495,58.550845],[-3.005005,58.635]]]]}")),
	?assertEqual({ok, [<<"uk">>]}, erl_spatial:index_intersects_mbr(Idx, {0, 52},{0, 52})),
	?assertEqual({ok, [<<"uk">>]}, erl_spatial:index_intersects(Idx, {0, 52},{0, 52})),

	% test flush
	?assertEqual(ok, erl_spatial:index_flush(Idx)).

index_limit_test() ->
	{ok, Idx} = erl_spatial:index_create([{?IDX_STORAGE, ?IDX_MEMORY}, {?IDX_RESULTLIMIT, 3}]),
	Pt = "{\"type\":\"Point\",\"coordinates\":[0.5, 0.5]}",
	lists:foreach(fun(X) ->
		 erl_spatial:index_insert(Idx, 
			list_to_binary(integer_to_list(X)), Pt)
		end,lists:seq(0, 4)),
	% test full dataset size
	?assertEqual({ok, 5}, erl_spatial:index_intersects_count(Idx, 
 													{0.5, 0.5}, {0.5, 0.5})),
 	% test limit function
 	?assertEqual({ok, [<<"2">>, <<"1">>, <<"0">>]}, erl_spatial:index_intersects(Idx,
 		{0.5, 0.5}, {0.5, 0.5})),

	erl_spatial:index_set_resultset_offset(Idx, 3),
 	?assertEqual({ok, [<<"4">>, <<"3">>]}, erl_spatial:index_intersects(Idx,
 		{0.5, 0.5}, {0.5, 0.5})).


 index_destroy_test() ->
    {ok, Idx} = erl_spatial:index_create([{?IDX_STORAGE, ?IDX_MEMORY}]),
    Pt = "{\"type\":\"Point\",\"coordinates\":[0.5, 0.5]}",
    ?assertEqual(ok, erl_spatial:index_insert(Idx, <<"point">>, Pt)),
    erl_spatial:index_flush(Idx),
	erl_spatial:index_destroy(Idx).

 index_persist_test() ->
 	Tmp = filename:absname("test"),
    {ok, Idx} = erl_spatial:index_create([{?IDX_STORAGE, ?IDX_DISK},
    	{?IDX_FILENAME, Tmp},
    	{?IDX_OVERWRITE, 1}]),

    % simple test of index preservation
    Pt = "{\"type\":\"Point\",\"coordinates\":[0.5, 0.5]}",
    ?assertEqual(ok, erl_spatial:index_insert(Idx, <<"point">>, Pt)),
    erl_spatial:index_flush(Idx),
    {ok, Idx2} = erl_spatial:index_create([{?IDX_STORAGE, ?IDX_DISK},
    	{?IDX_FILENAME, Tmp},
    	{?IDX_OVERWRITE, 0}]),
	?assertEqual({ok, 1}, erl_spatial:index_intersects_count(Idx, 
 													{0.5, 0.5}, {0.5, 0.5})),
	erl_spatial:index_destroy(Idx),
	erl_spatial:index_destroy(Idx2).

% benchmarks - tests to make sure everything is going faster enough
% currently disabled and not maintained, here for reference
% The python benchmarks don't use wkb_writer or geos
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