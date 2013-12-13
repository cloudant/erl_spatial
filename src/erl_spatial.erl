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
-module(erl_spatial).

-include("erl_spatial.hrl").

-on_load(init/0).

-export([index_create/0, index_create/1,
			index_insert/3, index_insert/4,
			index_intersects_count/3, index_intersects_mbr/3, index_intersects_mbr/5,
			index_intersects/2, index_intersects/4, index_intersects/3, index_intersects/5,
			index_contains/2, index_contains/4, index_contains/3, index_contains/5,
			index_contains_properly/2, index_contains_properly/4, index_contains_properly/3, index_contains_properly/5,
			index_covered_by/2, index_covered_by/4, index_covered_by/3, index_covered_by/5,
			index_covers/2, index_covers/4, index_covers/3, index_covers/5,
			index_crosses/2, index_crosses/4, index_crosses/3, index_crosses/5,
			index_disjoint/2, index_disjoint/4, index_disjoint/3, index_disjoint/5,
			index_overlaps/2, index_overlaps/4, index_overlaps/3, index_overlaps/5,
			index_touches/2, index_touches/4, index_touches/3, index_touches/5,
			index_within/2, index_within/4, index_within/3, index_within/5,
			index_bounds/1, index_delete/3, index_delete/4, 
			index_get_resultset_limit/1, index_set_resultset_limit/2,
			index_get_resultset_offset/1, index_set_resultset_offset/2,
			index_destroy/1, index_flush/1, sidx_version/0, geos_version/0,
			get_centre/1, get_centre/2]).

get_centre(Json) ->
	{ok, WKB} = wkb_writer:geojson_to_wkb(Json),
	get_centre(WKB, nif).

get_centre(WKB, nif) ->
	erlang:nif_error(not_loaded).

index_create() ->
	index_create([{?IDX_STORAGE, ?IDX_MEMORY},
		{?IDX_RESULTLIMIT, ?IDX_DEFAULTLIMIT}]).

index_create(_Props) ->
	erlang:nif_error(not_loaded).

% libspatialindex call
index_intersects_count(_Idx, _Min, _Max) ->
	erlang:nif_error(not_loaded).

% libspatialindex and geos

% minimum bounding rectangle (mbr) is a convenience function
index_intersects_mbr(Idx, Min, Max) ->
	index_intersects_mbr(Idx, Min, Max, 0, 0).

index_intersects_mbr(Idx, Min, Max, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Min, Max, ReqCrs, DbCrs, ?ST_INTERSECTS_MBR).

% Request is either WKT or {_X, _Y, _Radius} or Min/Max
index_intersects(Idx, Request) ->
	index_intersects(Idx, Request, 0, 0).

index_intersects(Idx, Request, ReqCrs, DbCrs) ->
    index_spatial_function(Idx, Request, ReqCrs, DbCrs, ?ST_INTERSECTS).

index_intersects(Idx, Min, Max) ->
    index_intersects(Idx, Min, Max, 0, 0).

index_intersects(Idx, Min, Max, ReqCrs, DbCrs) ->
    index_spatial_function(Idx, Min, Max, ReqCrs, DbCrs, ?ST_INTERSECTS).

% contains
index_contains(Idx, Request) ->
	index_contains(Idx, Request, 0, 0).

index_contains(Idx, Request, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Request, ReqCrs, DbCrs, ?ST_CONTAINS).

index_contains(Idx, Min, Max) ->
	index_contains(Idx, Min, Max, 0, 0).

index_contains(Idx, Min, Max, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Min, Max, ReqCrs, DbCrs, ?ST_CONTAINS).

% contains properly
index_contains_properly(Idx, Request) ->
	index_contains_properly(Idx, Request, 0, 0).

index_contains_properly(Idx, Request, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Request, ReqCrs, DbCrs, ?ST_CONTAINS_PROPERLY).

index_contains_properly(Idx, Min, Max) ->
	index_contains_properly(Idx, Min, Max, 0, 0).

index_contains_properly(Idx, Min, Max, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Min, Max, ReqCrs, DbCrs, ?ST_CONTAINS_PROPERLY).

% covered by
index_covered_by(Idx, Request) ->
	index_covered_by(Idx, Request, 0, 0).

index_covered_by(Idx, Request, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Request, ReqCrs, DbCrs, ?ST_COVERED_BY).

index_covered_by(Idx, Min, Max) ->
	index_covered_by(Idx, Min, Max, 0, 0).

index_covered_by(Idx, Min, Max, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Min, Max, ReqCrs, DbCrs, ?ST_COVERED_BY).

% covers
index_covers(Idx, Request) ->
	index_covers(Idx, Request, 0, 0).

index_covers(Idx, Request, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Request, ReqCrs, DbCrs, ?ST_COVERS).

index_covers(Idx, Min, Max) ->
	index_covers(Idx, Min, Max, 0, 0).

index_covers(Idx, Min, Max, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Min, Max, ReqCrs, DbCrs, ?ST_COVERS).

% crosses
index_crosses(Idx, Request) ->
	index_crosses(Idx, Request, 0, 0).

index_crosses(Idx, Request, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Request, ReqCrs, DbCrs, ?ST_CROSSES).

index_crosses(Idx, Min, Max) ->
	index_crosses(Idx, Min, Max, 0, 0).

index_crosses(Idx, Min, Max, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Min, Max, ReqCrs, DbCrs, ?ST_CROSSES).

% disjoint
index_disjoint(Idx, Request) ->
	index_disjoint(Idx, Request, 0, 0).

index_disjoint(Idx, Request, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Request, ReqCrs, DbCrs, ?ST_DISJOINT).

index_disjoint(Idx, Min, Max) ->
	index_disjoint(Idx, Min, Max, 0, 0).

index_disjoint(Idx, Min, Max, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Min, Max, ReqCrs, DbCrs, ?ST_DISJOINT).

% overlaps
index_overlaps(Idx, Request) ->
	index_overlaps(Idx, Request, 0, 0).

index_overlaps(Idx, Request, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Request, ReqCrs, DbCrs, ?ST_OVERLAPS).

index_overlaps(Idx, Min, Max) ->
	index_overlaps(Idx, Min, Max, 0, 0).

index_overlaps(Idx, Min, Max, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Min, Max, ReqCrs, DbCrs, ?ST_OVERLAPS).

% touches
index_touches(Idx, Request) ->
	index_touches(Idx, Request, 0, 0).

index_touches(Idx, Request, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Request, ReqCrs, DbCrs, ?ST_TOUCHES).

index_touches(Idx, Min, Max) ->
	index_touches(Idx, Min, Max, 0, 0).

index_touches(Idx, Min, Max, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Min, Max, ReqCrs, DbCrs, ?ST_TOUCHES).

% within
index_within(Idx, Request) ->
	index_within(Idx, Request, 0, 0).

index_within(Idx, Request, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Request, ReqCrs, DbCrs, ?ST_WITHIN).

index_within(Idx, Min, Max, ReqCrs, DbCrs) ->
	index_spatial_function(Idx, Min, Max, ReqCrs, DbCrs, ?ST_WITHIN).

index_within(Idx, Min, Max) ->
	index_within(Idx, Min, Max, 0, 0).

index_spatial_function(_Idx, _Request, _ReqCrs, _DbCrs, _FunName) ->
	erlang:nif_error(not_loaded).

index_spatial_function(_Idx, _Min, _Max, _ReqCrs, _DbCrs, _FunName) ->
	erlang:nif_error(not_loaded).

% end of geos

index_bounds(_Idx) ->
	erlang:nif_error(not_loaded).

index_get_resultset_limit(_Idx) ->
	erlang:nif_error(not_loaded).

index_set_resultset_limit(_Idx, _Val) ->
	erlang:nif_error(not_loaded).

index_get_resultset_offset(_Idx) ->
	erlang:nif_error(not_loaded).

index_set_resultset_offset(_Idx, _Val) ->
	erlang:nif_error(not_loaded).

index_flush(_Idx) ->
	erlang:nif_error(not_loaded).

index_destroy(_Idx) ->
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