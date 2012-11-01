-module(erl_spatial_tests).

-include_lib("eunit/include/eunit.hrl").

index_create_test() ->
	% test create an in-memory r-tree
	?assertEqual({ok, <<>>}, erl_spatial:index_create()).

index_test() ->
	Min = {0, 0},
	Max = {1, 1},
	DocId = <<"test">>,

	{ok, Idx} = erl_spatial:index_create(),
	?assertEqual(ok, erl_spatial:index_insert(Idx, DocId, Min, Max)),

	% check we hit and then check the intersection for the original
	?assertEqual({ok, 1}, erl_spatial:index_intersects_count(Idx, Min, Max)),
	?assertEqual({ok, [DocId]}, erl_spatial:index_intersects(Idx, Min, Max)),

	% check the misses
	?assertEqual({ok, 0}, 
		erl_spatial:index_intersects_count(Idx, {2, 2}, {3, 3})),

	% clean up
	?assertEqual(ok, erl_spatial:index_delete(Idx, DocId, Min, Max)),
	?assertEqual({ok, 0}, erl_spatial:index_intersects_count(Idx, Min, Max)).