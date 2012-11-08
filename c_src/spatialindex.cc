// Copyright 2012 Cloudant
#include "erl_spatial.h"

// libspatialindex - https://github.com/libspatialindex/libspatialindex
#include "spatialindex/capi/sidx_api.h"
// geos - http://trac.osgeo.org/geos/
#include "geos_c.h"

#define MAXBUFLEN	1024

static ErlNifResourceType *index_type = NULL;

struct idx_state_t
{
	IndexH index;
	GEOSContextHandle_t geosCtx;
};

typedef struct idx_state_t idx_state;

static struct {
    ERL_NIF_TERM ok;
    ERL_NIF_TERM error;
} idx_atoms;


void
notice(const char *fmt, ...) {
	fprintf(stdout, "%s\n", fmt);
	fflush(stdout);
}

void
error(const char *fmt, ...) {
	fprintf(stderr, "%s\n", fmt);
	fflush(stderr);
}

void
idx_state_dtor(ErlNifEnv* env, void* obj);

int
get_min_max_tuple(ErlNifEnv* env, double* mins, double* maxs, 
	const ERL_NIF_TERM* min_tuple, const ERL_NIF_TERM* max_tuple, int dims);

int
get_min_max(GEOSContextHandle_t geosCtx, const GEOSGeometry* mbr,
	double* const mins, double* const maxs, int dims);

int64_t 
hash(const unsigned char* szVal);

RTError
set_property(ErlNifEnv* env, int propType, ERL_NIF_TERM term, 
														IndexPropertyH props);

int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
	ErlNifResourceFlags flags = 
				(ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
	ErlNifResourceType* res;

	res = enif_open_resource_type(env, NULL, "index_type", idx_state_dtor,
																 flags, NULL);

	if (res == NULL)
	{
		return -1;
	}
	else
	{
		assert(index_type == NULL);
		index_type = res;

		idx_atoms.ok = enif_make_atom(env, "ok");
		idx_atoms.error = enif_make_atom(env, "error");

		return 0;
	}
}

int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return load(env, priv, info);
}

void
unload(ErlNifEnv* env, void* priv)
{
    enif_free(priv);
    return;
}

ERL_NIF_TERM
index_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	idx_state* pState;
	ERL_NIF_TERM item, items;
	const ERL_NIF_TERM* tuple;
	int arity;

	ERL_NIF_TERM result;

	if (!enif_is_list(env, argv[0]))
		return enif_make_badarg(env);

	IndexPropertyH props = IndexProperty_Create();

	// parse index property options
	items = argv[0];
	while(enif_get_list_cell(env, items, &item, &items)) {
		if(enif_get_tuple(env, item, &arity, &tuple) && (arity == 2)) {
			int propType;
			if (enif_get_int(env, tuple[0], &propType))
			{
				set_property(env, propType, tuple[1], props);		
			}
			else
			{
				IndexProperty_Destroy(props);
				return enif_make_tuple2(env, idx_atoms.error,
					enif_make_string(env, "Unrecognized property type",
						ERL_NIF_LATIN1));
			}
		}
		else
		{
			IndexProperty_Destroy(props);	
			return enif_make_tuple2(env, idx_atoms.error, 
				enif_make_string(env, 
				  "Arguments are required to be a list of tuples, {Key, Value}",
				  ERL_NIF_LATIN1));
		}
	}

	pState = (idx_state*)enif_alloc_resource(index_type, sizeof(idx_state));

	IndexH handle = Index_Create(props);

	if (Index_IsValid(handle))
	{
		pState->index = handle;

		pState->geosCtx = initGEOS_r(notice, error);
		GEOS_setWKBByteOrder_r(pState->geosCtx, GEOS_WKB_XDR);

		result = enif_make_resource(env, pState);
		enif_release_resource(pState);

		return enif_make_tuple2(env, idx_atoms.ok, result);
	}
	else
		return enif_make_tuple2(env, idx_atoms.error, 
			enif_make_string(env, 
				"Unable to make a valid index", ERL_NIF_LATIN1));
}

ERL_NIF_TERM 
index_insert_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	idx_state *pState;
	unsigned char* pszDocId;
	ErlNifBinary bin, wkb;
	GEOSGeometry* geom;
	int dims;
	int doc_len;

	if (!enif_get_resource(env, argv[0], index_type, (void **) &pState))
		return enif_make_badarg(env);
	
	if (enif_inspect_binary(env, argv[1], &bin))
	{
		pszDocId = (unsigned char*)malloc(bin.size + 1);
		memcpy(pszDocId, bin.data, bin.size);
		pszDocId[bin.size] = 0;
		doc_len = bin.size + 1;		
	}
	else
		return enif_make_tuple2(env, idx_atoms.error, 
				enif_make_string(env, 
				"Unable to parse Id", ERL_NIF_LATIN1));

	// get wkb data and calculate min and max from MBR
	if (enif_inspect_binary(env, argv[2], &wkb) &&
		(geom = GEOSGeomFromWKB_buf_r(pState->geosCtx, 
			wkb.data, wkb.size)) != NULL)
	{
		GEOSGeometry* mbr;
		double* mins;
		double* maxs;
		unsigned char* pData;

		mbr = GEOSEnvelope_r(pState->geosCtx, geom); // mbr is a polygon
		dims = GEOSGeom_getCoordinateDimension_r(pState->geosCtx, mbr);
		mins = (double*)malloc(dims * sizeof(double));
		maxs = (double*)malloc(dims * sizeof(double));

		get_min_max(pState->geosCtx, mbr, mins, maxs, dims);

		// doc id is null terminated join wkb data and doc id together
		pData = (unsigned char*)malloc(wkb.size + doc_len);
		
		memcpy(pData, (unsigned char*)pszDocId, doc_len);
		memcpy(pData + doc_len + 1, wkb.data, wkb.size);

		if (Index_InsertData(pState->index,
			hash(pszDocId), mins, maxs, dims, 
			(uint8_t *)pData, wkb.size + doc_len) != RT_None)
		{
			char buf[MAXBUFLEN];
			
			free(mins);
			free(maxs);
			free(pData);
			free(pszDocId);
			GEOSGeom_destroy_r(pState->geosCtx, geom);
			GEOSGeom_destroy_r(pState->geosCtx, mbr);
			sprintf(buf, "unable to insert document %s into index", pszDocId);
			return enif_make_tuple2(env, idx_atoms.error, 
				enif_make_string(env, buf, ERL_NIF_LATIN1));
		}

		free(mins);
		free(maxs);
		free(pData);
		free(pszDocId);
		GEOSGeom_destroy_r(pState->geosCtx, geom);
		GEOSGeom_destroy_r(pState->geosCtx, mbr);
		return idx_atoms.ok;
	}
	else
	{
		free(pszDocId);
		return enif_make_tuple2(env, idx_atoms.error,
			enif_make_string(env, "Unable to parse WKB data", ERL_NIF_LATIN1));
	}
}

ERL_NIF_TERM
index_intersects_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	idx_state *pState;
	const ERL_NIF_TERM* min_tuple;
	const ERL_NIF_TERM* max_tuple;
	int min_arity, max_arity;
	uint64_t nResults = 0;

	if (!enif_get_resource(env, argv[0], index_type, (void **) &pState))
		return enif_make_badarg(env);

	if (!enif_get_tuple(env, argv[1], &min_arity, &min_tuple))
		return enif_make_tuple2(env, idx_atoms.error,
			enif_make_string(env, "Unable to parse min tuple", ERL_NIF_LATIN1));

	if (!enif_get_tuple(env, argv[2], &max_arity, &max_tuple))
		return enif_make_tuple2(env, idx_atoms.error, 
			enif_make_string(env, "Unable to parse max tuple", ERL_NIF_LATIN1));

	if (!(min_arity == max_arity))
		return enif_make_tuple2(env, idx_atoms.error,
			enif_make_string(env, "min and max tuple arity needs to be equal", 
			ERL_NIF_LATIN1));

	double mins[min_arity];
	double maxs[max_arity];

	if (get_min_max_tuple(env, mins, maxs, min_tuple, max_tuple, min_arity) 
																	!= RT_None)
		return enif_make_tuple2(env, idx_atoms.error, 
			enif_make_string(env, "error getting min and max values", 
				ERL_NIF_LATIN1));

	if (Index_Intersects_count(pState->index, mins, 
								maxs, min_arity, &nResults) != RT_None)
		return enif_make_tuple2(env, idx_atoms.error, 
			enif_make_string(env, "unable to execute query", ERL_NIF_LATIN1));

	return enif_make_tuple2(env, idx_atoms.ok,  enif_make_uint64(env, nResults));
}

ERL_NIF_TERM
index_intersects(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	idx_state *pState;
	const ERL_NIF_TERM* min_tuple;
	const ERL_NIF_TERM* max_tuple;
	int min_arity, max_arity;
	uint64_t nResults;
	IndexItemH* items;
	ERL_NIF_TERM resultList;

	if (!enif_get_resource(env, argv[0], index_type, (void **) &pState))
		return enif_make_badarg(env);

	if (!enif_get_tuple(env, argv[1], &min_arity, &min_tuple))
		return enif_make_tuple2(env, idx_atoms.error, 
			enif_make_string(env, "Unable to parse min tuple", ERL_NIF_LATIN1));

	if (!enif_get_tuple(env, argv[2], &max_arity, &max_tuple))
		return enif_make_tuple2(env, idx_atoms.error, 
			enif_make_string(env, "Unable to parse max tuple", ERL_NIF_LATIN1));

	if (!(min_arity == max_arity))
		return enif_make_tuple2(env, idx_atoms.error, 
			enif_make_string(env, "min and max tuple arity needs to be equal",
								 ERL_NIF_LATIN1));

	double mins[min_arity];
	double maxs[max_arity];

	if (get_min_max_tuple(env, mins, maxs, min_tuple, max_tuple, min_arity)
																	!= RT_None)
		return enif_make_tuple2(env, idx_atoms.error, 
			enif_make_string(env, "error getting min and max values", 
								ERL_NIF_LATIN1));

	if (Index_Intersects_obj(pState->index, mins, maxs,
							 min_arity, &items, &nResults) != RT_None)
		return enif_make_tuple2(env, idx_atoms.error, 
			enif_make_string(env, "unable to execute query", ERL_NIF_LATIN1));

	resultList = enif_make_list(env, 0);

	for (int i = 0; i < nResults; i++)
	{
		unsigned char* data;
		uint64_t len;
		IndexItemH item = items[i];

		if (IndexItem_GetData(item, (uint8_t **)&data, &len) == RT_None)
		{
			ErlNifBinary bin;
			int docid_len;

			// data is a NULL terminated string followed by WKB
			docid_len = strlen((char*)data);

			if (enif_alloc_binary(docid_len, &bin))
			{
				memcpy(bin.data, data, docid_len);
				ERL_NIF_TERM head = enif_make_binary(env, &bin);
				resultList = enif_make_list_cell(env, head, resultList);
			}
			else
			{
				free(data);
				IndexItem_Destroy(item);
				char buf[MAXBUFLEN];
				data[len] = 0;
				sprintf(buf, "unable to assign doc id %s to erlang term", data);
				return enif_make_tuple2(env, idx_atoms.error, 
					enif_make_string(env, buf, ERL_NIF_LATIN1));
			}

			free(data);
			IndexItem_Destroy(item);

		}
		else
		{
			return enif_make_tuple2(env, idx_atoms.error, 
				enif_make_string(env, "unable to execute query", 
					ERL_NIF_LATIN1));
		}
	}

	return enif_make_tuple2(env, idx_atoms.ok, resultList);;
}

ERL_NIF_TERM index_delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	idx_state *pState;
	unsigned char* pszDocId;
	ErlNifBinary bin, wkb;
	GEOSGeometry* geom;
	int dims;
 
	if (!enif_get_resource(env, argv[0], index_type, (void **) &pState))
		return enif_make_badarg(env);
	
	if (enif_inspect_binary(env, argv[1], &bin) && (bin.size < MAXBUFLEN))
	{
		pszDocId = (unsigned char*)malloc(bin.size + 1);
		memcpy(pszDocId, bin.data, bin.size);
		pszDocId[bin.size] = 0;
	}
	else
		return enif_make_tuple2(env, idx_atoms.error, 
				enif_make_string(env, 
				"Unable to parse Id", ERL_NIF_LATIN1));

	// get wkb data and calculate min and max from MBR
	if (enif_inspect_binary(env, argv[2], &wkb) &&
		(geom = GEOSGeomFromWKB_buf_r(pState->geosCtx, 
			wkb.data, wkb.size)) != NULL)
	{
		GEOSGeometry* mbr;
		double* mins;
		double* maxs;

		mbr = GEOSEnvelope_r(pState->geosCtx, geom); // mbr is a polygon
		dims = GEOSGeom_getCoordinateDimension_r(pState->geosCtx, mbr);
		mins = (double*)malloc(dims * sizeof(double));
		maxs = (double*)malloc(dims * sizeof(double));

		get_min_max(pState->geosCtx, mbr, mins, maxs, dims);

		if (Index_DeleteData(pState->index, hash(pszDocId), mins,
				maxs, dims) != RT_None)
		{
			char buf[MAXBUFLEN];

			free(mins);
			free(maxs);
			free(pszDocId);
			GEOSGeom_destroy_r(pState->geosCtx, geom);
			GEOSGeom_destroy_r(pState->geosCtx, mbr);

			sprintf(buf, "unable to delete document %s from index", pszDocId);
			return enif_make_tuple2(env, idx_atoms.error, 
						enif_make_string(env, buf, ERL_NIF_LATIN1));

		}
		else
		{
			free(mins);
			free(maxs);
			free(pszDocId);
			GEOSGeom_destroy_r(pState->geosCtx, geom);
			GEOSGeom_destroy_r(pState->geosCtx, mbr);

			return idx_atoms.ok;
		}
	}
	else
	{
		free(pszDocId);
		return enif_make_tuple2(env, idx_atoms.error,
			enif_make_string(env, "Unable to parse WKB data", ERL_NIF_LATIN1));
	}
}

ERL_NIF_TERM
geos_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	return enif_make_string(env, GEOSversion(), ERL_NIF_LATIN1);
}


ERL_NIF_TERM
sidx_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	return enif_make_string(env, SIDX_Version(), ERL_NIF_LATIN1);
}

RTError set_property(ErlNifEnv* env, int propType, ERL_NIF_TERM term, 
														IndexPropertyH props)
{
	int v = 0;
	uint32_t uv = 0;
	double d = 0;

	RTError result = RT_None;

	switch(propType)
	{
		case IndexType:
			RTIndexType type;
			if (enif_get_int(env, term, &v))
			{
				type = static_cast<RTIndexType>(v);
				IndexProperty_SetIndexType(props, type);
			}
			else result = RT_Failure;	
			break;
		case Dimension:
			if (enif_get_uint(env, term, &uv))
			{
				IndexProperty_SetDimension(props, uv);
			}
			else result = RT_Failure;	
			break;
		case Variant:
			RTIndexVariant variant;
			if (enif_get_int(env, term, &v))
			{
				variant = static_cast<RTIndexVariant>(v);
				IndexProperty_SetIndexVariant(props, variant);
			}
			else result = RT_Failure;	
			break;
		case Storage:
			RTStorageType storage;
			if (enif_get_int(env, term, &v))
			{
				storage = static_cast<RTStorageType>(v);
				IndexProperty_SetIndexStorage(props, storage);
			}
			else result = RT_Failure;	
			break;
		case PageSize:
			if (enif_get_uint(env, term, &uv))
			{
				IndexProperty_SetPagesize(props, uv);
			}
			else result = RT_Failure;	
			break;
		case IndexCapacity:
			if (enif_get_uint(env, term, &uv))
			{
				IndexProperty_SetIndexCapacity(props, uv);
			}
			else result = RT_Failure;	
			break;
		case LeafCapacity:
			if (enif_get_uint(env, term, &uv))
			{
				IndexProperty_SetLeafCapacity(props, uv);
			}
			else result = RT_Failure;	
			break;
		case LeafPoolCapacity:
			if (enif_get_uint(env, term, &uv))
			{
				IndexProperty_SetLeafPoolCapacity(props, uv);
			}
			else result = RT_Failure;	
			break;
		case IndexPoolCapacity:
			if (enif_get_uint(env, term, &uv))
			{
				IndexProperty_SetIndexPoolCapacity(props, uv);
			}
			else result = RT_Failure;	
			break;
		case RegionPoolCapacity:
			if (enif_get_uint(env, term, &uv))
			{
				IndexProperty_SetRegionPoolCapacity(props, uv);
			}
			else result = RT_Failure;	
			break;
		case PointPoolCapacity:
			if (enif_get_uint(env, term, &uv))
			{
				IndexProperty_SetPointPoolCapacity(props, uv);
			}
			else result = RT_Failure;	
			break;
		case BufferingCapacity:
			if (enif_get_uint(env, term, &uv))
			{
				IndexProperty_SetBufferingCapacity(props, uv);
			}
			else result = RT_Failure;	
			break;
		case TightMBRS:
			if (enif_get_uint(env, term, &uv))
			{
				IndexProperty_SetEnsureTightMBRs(props, uv);
			}
			else result = RT_Failure;	
			break;
		case Overwrite:
			if (enif_get_uint(env, term, &uv))
			{
				IndexProperty_SetOverwrite(props, uv);
			}
			else result = RT_Failure;				
			break;
		case NearMinimumOverlapFactor:
			if (enif_get_uint(env, term, &uv))
			{
				IndexProperty_SetNearMinimumOverlapFactor(props, uv);
			}
			else result = RT_Failure;	
			break;
		case WriteThrough:
			if (enif_get_uint(env, term, &uv))
			{
				IndexProperty_SetWriteThrough(props, uv);
			}
			else result = RT_Failure;	
			break;
		case FillFactor:
			if (enif_get_double(env, term, &d))
			{
				IndexProperty_SetFillFactor(props, d);
			}
			else result = RT_Failure;	
			break;
		case SplitDistnFactor:
			if (enif_get_double(env, term, &d))
			{
				IndexProperty_SetSplitDistributionFactor(props, d);
			}
			else result = RT_Failure;	
			break;	
		case TPRHorizon:
			if (enif_get_double(env, term, &d))
			{
				IndexProperty_SetTPRHorizon(props, d);
			}
			else result = RT_Failure;	
			break;
		case ReinsertFactor:
			if (enif_get_double(env, term, &d))
			{
				IndexProperty_SetReinsertFactor(props, d);
			}
			else result = RT_Failure;	
			break;
		case FileName:
			char szFileName[MAXBUFLEN];
			if (enif_get_string(env, term, szFileName, MAXBUFLEN, 
									ERL_NIF_LATIN1) > 0)
			{
				IndexProperty_SetFileName(props, szFileName);
			}
			else result = RT_Failure;	
			break;
		case FileNameExtDat:
			char szExtDat[MAXBUFLEN];
			if (enif_get_string(env, term, szExtDat, MAXBUFLEN, 
									ERL_NIF_LATIN1)  > 0)
			{
				IndexProperty_SetFileNameExtensionDat(props, szExtDat);
			}
			else result = RT_Failure;	
			break;
		case FileNameExtIdx:
			char szExtIdx[MAXBUFLEN];
			if (enif_get_string(env, term, szExtIdx, MAXBUFLEN,
									 ERL_NIF_LATIN1)  > 0)
			{
				IndexProperty_SetFileNameExtensionIdx(props, szExtIdx);
			}
			else result = RT_Failure;	
			break;
		case IndexId:
			int64_t i;
			if (enif_get_int64(env, term, (ErlNifSInt64*) &i))
			{				
				IndexProperty_SetIndexID(props, i);
			}
			else result = RT_Failure;	
			break;
		default:
			result = RT_Failure;
	}

	return result;
}

int
get_min_max(GEOSContextHandle_t geosCtx, const GEOSGeometry* mbr,
	double* const mins, double* const maxs, int arity)
{
	int n;
	const GEOSCoordSequence *cs;
	
	if (GEOSisValid_r(geosCtx, mbr) == 1)
	{
		// loop mbr to find min and max
		n = GEOSGetNumCoordinates_r(geosCtx, mbr);
		cs = GEOSGeom_getCoordSeq_r(geosCtx, mbr);

		for (int i = 0; i < arity; i++)
		{
			double v;
			// initialise min and maxs
			GEOSCoordSeq_getOrdinate_r(geosCtx, cs, 0, i, &v);
			mins[i] = v;
			maxs[i] = v;

			for (int j = 1; j < n; j++)
			{
				GEOSCoordSeq_getOrdinate_r(geosCtx, cs, 0, i, &v);
				if (v > maxs[i])
					maxs[i] = v;

				if (v < mins[i])
					mins[i] = v;
			}
		}
		return RT_None;
	}
	else return RT_Failure;
}

int
get_min_max_tuple(ErlNifEnv* env, double* mins, double* maxs,
	 const ERL_NIF_TERM* min_tuple, const ERL_NIF_TERM* max_tuple, int dims)
{
	for (int i = 0; i < dims; i++)
	{
		double d1, d2;
		int v1, v2;
		if (!enif_get_double(env, min_tuple[i], &d1))
		{
			if (enif_get_int(env, min_tuple[i], &v1))
				mins[i] = v1;
			else
				return RT_Failure;
		}
		else
			mins[i] = d1;

		if (!enif_get_double(env, max_tuple[i], &d2))
		{
			if (enif_get_int(env, max_tuple[i], &v2))
				maxs[i] = v2;
			else
				return RT_Failure;
		}
		else
			maxs[i] = d2;
	}  	

	return RT_None;
}

int64_t 
hash(const unsigned char* szVal)
{
	int64_t h = 0;
	while (*szVal)
		h = h << 1 ^ *szVal++;

	return h;
}

void
idx_state_dtor(ErlNifEnv* env, void* obj)
{
	idx_state* pState = (idx_state*) obj;
	
	if (pState->geosCtx)
		finishGEOS_r(pState->geosCtx);

	if (pState->index)
		Index_Destroy(pState->index);
}
