-module(erl_spatial).

-include("erl_spatial.hrl").

-on_load(init/0).

-export([index_create/0, index_create/1, index_insert/4, index_intersects_count/3, index_intersects/3, index_delete/4, sidx_version/0]).

index_create() ->
	index_create([{?IDX_STORAGE, ?IDX_MEMORY}]).

index_create(_Props) ->
	erlang:nif_error(not_loaded).

index_insert(Idx, Id, Min, Max) when is_list(Id) ->
	index_insert(Idx, list_to_binary(Id), Min, Max);

index_insert(_Idx, _Id, _Min, _Max) ->
	erlang:nif_error(not_loaded).	

index_intersects_count(_Idx, _Min, _Max) ->
	erlang:nif_error(not_loaded).

index_intersects(_Idx, _Min, _Max) ->
	erlang:nif_error(not_loaded).

index_delete(Idx, Id, Min, Max) when is_list(Id) ->
	index_delete(Idx, list_to_binary(Id), Min, Max);

index_delete(_Idx, _Id,  _Min, _Max) ->
	erlang:nif_error(not_loaded).

sidx_version() ->
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
