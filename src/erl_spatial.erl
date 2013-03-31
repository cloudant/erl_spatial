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

-export([index_create/0, index_create/1, index_create/2, 
			index_insert/3, index_insert/4,
			index_intersects_count/3, index_intersects_mbr/3,
			index_intersects_mbr/5, index_intersects/2, index_intersects/4, 
			index_intersects/3, index_intersects/5, index_bounds/1,
			index_delete/3, index_delete/4, 
			index_destroy/1, sidx_version/0, geos_version/0]).

index_create() ->
	index_create([{?IDX_STORAGE, ?IDX_MEMORY}]).

index_create(_Props) ->
	erlang:nif_error(not_loaded).

index_create(FileName, CRS) when is_binary(FileName) ->
	index_create(binary_to_list(FileName), CRS);

index_create(FileName, _CRS) ->
	% TODO include CRS to support on the fly reprojection
	index_create([{?IDX_FILENAME, FileName}]).

index_intersects_count(_Idx, _Min, _Max) ->
	erlang:nif_error(not_loaded).

index_intersects_mbr(Idx, Min, Max) ->
	index_intersects_mbr(Idx, Min, Max, 0, 0).

index_intersects_mbr(_Idx, _Min, _Max, _ReqCrs, _DbCrs) ->
	erlang:nif_error(not_loaded).

% Request is either WKT or {_Lon, _Lat, _Radius}
index_intersects(Idx, Request) ->
	index_intersects(Idx, Request, 0, 0).

index_intersects(_Idx, _Request, _ReqCrs, _DbCrs) ->
 	erlang:nif_error(not_loaded).

index_intersects(Idx, Min, Max) ->
	index_intersects(Idx, Min,  Max, 0, 0).

index_intersects(_Idx, _Min, _Max, _ReqCrs, _DbCrs) ->
	erlang:nif_error(not_loaded).

index_bounds(_Idx) ->
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