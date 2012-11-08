%% Copyright 2012 Cloudant
-module(erl_spatial).

-include("erl_spatial.hrl").

-on_load(init/0).

-export([index_create/0, index_create/1, index_create/2, 
			index_insert/3, index_insert/4,
			index_intersects_count/3, index_intersects/3,
			index_delete/3, index_delete/4, sidx_version/0, geos_version/0]).

index_create() ->
	index_create([{?IDX_STORAGE, ?IDX_MEMORY}]).

index_create(_Props) ->
	erlang:nif_error(not_loaded).

index_create(FileName, CRS) is_binary(FileName) ->
	index_create(binary_to_list(FileName), CRS);

index_create(FileName, CRS) ->
	% TODO include CRS to support on the fly reprojection
	index_create([{?IDX_FILENAME, FileName}]).

index_intersects_count(_Idx, _Min, _Max) ->
	erlang:nif_error(not_loaded).

index_intersects(_Idx, _Min, _Max) ->
	erlang:nif_error(not_loaded).

sidx_version() ->
	erlang:nif_error(not_loaded).

geos_version() ->
	erlang:nif_error(not_loaded).

index_insert(Idx, Id, Json) ->
	{ok, WKB} = wkb_writer:geojson_to_wkb(Json),
	index_insert(Idx, Id, WKB, nif).

index_insert(_Idx, _Id, _Data, nif) ->
	erlang:nif_error(not_loaded).	

index_delete(Idx, Id, Json) ->
	{ok, WKB} = wkb_writer:geojson_to_wkb(Json),
	index_delete(Idx, Id, WKB, nif).

index_delete(_Idx, _Id, _Data, nif) ->
	erlang:nif_error(not_loaded).
%% -------------------------------------------------------------------------
%% on_load callback
%% -------------------------------------------------------------------------
init() ->
	PrivDir = case code:priv_dir(?MODULE) of
		{error, _} ->
			EbinDir = filename:dirname(code:which(?MODULE)),
			AppPath = filename:dirname(EbinDir),
			filename:join(AppPath, "priv");
		Path ->
			Path
	end,
	erlang:load_nif(filename:join(PrivDir, "erl_spatial"), 0).