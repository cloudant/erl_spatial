// Copyright 2012 Cloudant
#include "erl_spatial.h"

// static int
// load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
// {
//     return 0;
// }

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return load(env, priv, info);
}

static void
unload(ErlNifEnv* env, void* priv)
{
    enif_free(priv);
    return;
}

//  name, arity and function pointer
static ErlNifFunc nif_funcs[] = {
    {"index_create", 1, index_create},
    {"index_insert", 4, index_insert_data},
    {"index_intersects_count", 3, index_intersects_count},
    {"index_intersects", 3, index_intersects},
    {"index_delete", 4, index_delete},
    {"sidx_version", 0, sidx_version}    
};



ERL_NIF_INIT(erl_spatial, nif_funcs, &load, &reload, &upgrade, &unload);