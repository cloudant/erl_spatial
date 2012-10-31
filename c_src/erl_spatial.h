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
	IndexId = 23
} IdxProperties;

int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info); 

ERL_NIF_TERM index_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM index_insert_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM index_intersects_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM index_intersects(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM index_delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM sidx_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

