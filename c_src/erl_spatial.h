// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at

//    http://www.apache.org/licenses/LICENSE-2.0

//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
//  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
//  License for the specific language governing permissions and limitations under
//  the License.

#include "erl_nif.h"

typedef enum
{
	IndexType = 0,
	Dimension = 1,
	Variant = 2,
	Storage = 3,
	PageSize = 4,
	IndexCapacity = 5,
	LeafCapacity = 6,
	LeafPoolCapacity = 7,
	IndexPoolCapacity = 8,
	RegionPoolCapacity = 9,
	PointPoolCapacity = 10,
	BufferingCapacity = 11,
	TightMBRS = 12,
	Overwrite = 13,
	NearMinimumOverlapFactor = 14,
	WriteThrough = 15,
	FillFactor = 16,
	SplitDistnFactor = 17,
	TPRHorizon = 18,
	ReinsertFactor = 19,
	FileName = 20,
	FileNameExtDat = 21,
	FileNameExtIdx = 22,
	IndexId = 23,
	ResultSetLimit = 24
} IdxProperties;

typedef enum {
	ST_INTERSECTS_MBR = 0,
	ST_INTERSECTS = 1,
	ST_CONTAINS = 2,
	ST_CONTAINS_PROPERLY = 3,
	ST_COVERED_BY = 4,
	ST_COVERS = 5,
	ST_CROSSES = 6,
	ST_DISJOINT = 7,
	ST_OVERLAPS = 8,
	ST_TOUCHES = 9,
	ST_WITHIN = 10,
	ST_NEAREST = 11,
	ST_TPINTERSECTS_MBR = 12,
	ST_TPNEAREST = 13
} SpatialFuns;

int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info);
int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info);
void unload(ErlNifEnv* env, void* priv);

ERL_NIF_TERM get_centre(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM index_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM index_insert_data(ErlNifEnv* env, int argc,
													const ERL_NIF_TERM argv[]);
ERL_NIF_TERM index_intersects_count(ErlNifEnv* env, int argc,
													const ERL_NIF_TERM argv[]);
ERL_NIF_TERM index_spatial_function(ErlNifEnv* env, int argc,
													const ERL_NIF_TERM argv[]);
ERL_NIF_TERM index_bounds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM index_delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM index_destroy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM index_flush(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM index_get_resultset_limit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM index_set_resultset_limit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM index_get_resultset_offset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM index_set_resultset_offset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM sidx_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM geos_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
