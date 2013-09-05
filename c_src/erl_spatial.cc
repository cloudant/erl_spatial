// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at

//    http://www.apache.org/licenses/LICENSE-2.0

//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
//  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
//  License for the specific language governing permissions and limitations under
//  the License.

#include "erl_spatial.h"

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

//  name, arity and function pointer
static ErlNifFunc nif_funcs[] = {
    {"index_create", 1, index_create},
    {"index_insert", 4, index_insert_data},
    {"index_intersects_count", 3, index_intersects_count},
    {"index_spatial_function", 5, index_spatial_function},
    {"index_spatial_function", 6, index_spatial_function},
    {"index_bounds", 1, index_bounds},
    {"index_delete", 4, index_delete},
    {"index_destroy", 1, index_destroy},
    {"index_flush", 1, index_flush},
    {"index_get_resultset_limit",1, index_get_resultset_limit},
    {"index_set_resultset_limit",2, index_set_resultset_limit},
    {"index_get_resultset_offset",1, index_get_resultset_offset},
    {"index_set_resultset_offset",2, index_set_resultset_offset},
    {"sidx_version", 0, sidx_version},
    {"geos_version", 0, geos_version}
};

ERL_NIF_INIT(erl_spatial, nif_funcs, &load, &reload, &upgrade, &unload);