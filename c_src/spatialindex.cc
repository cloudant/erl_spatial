// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at

//    http://www.apache.org/licenses/LICENSE-2.0

//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
//  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
//  License for the specific language governing permissions and limitations under
//  the License.
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "erl_spatial.h"

// libspatialindex - https://github.com/libspatialindex/libspatialindex
#include "spatialindex/capi/sidx_api.h"
// geos - http://trac.osgeo.org/geos/
#include "geos_c.h"
// csmap - http://trac.osgeo.org/csmap/
#include "cs_map.h"
#include "csNameMapperSupport.h"

#define MAXBUFLEN	1024

static ErlNifResourceType *index_type = NULL;
static ErlNifMutex* csMapMutex = NULL;

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
	va_list ap;
    fprintf( stdout, "NOTICE: ");
	va_start (ap, fmt);
    vfprintf( stdout, fmt, ap);
    va_end(ap);
    fprintf( stdout, "\n" );
}

void
error(const char *fmt, ...) {
	va_list ap;
    fprintf( stdout, "ERROR: ");
	va_start (ap, fmt);
    vfprintf( stdout, fmt, ap);
    va_end(ap);
    fprintf( stdout, "\n" );
}

void
idx_state_dtor(ErlNifEnv* env, void* obj);

int
get_min_max_seq(GEOSContextHandle_t geosCtx, const GEOSCoordSequence* cs, 
	double* const mins, double* const maxs, int dims, int n);

int
get_min_max_tuple(ErlNifEnv* env, double* mins, double* maxs, 
	const ERL_NIF_TERM* min_tuple, const ERL_NIF_TERM* max_tuple, int dims);

int
get_min_max(GEOSContextHandle_t geosCtx, const GEOSGeometry* geom,
	double* const mins, double* const maxs, int dims);

int 
get_number(ErlNifEnv* env, const ERL_NIF_TERM* tuple, int pos, double* v);

ERL_NIF_TERM
spatial_function(ErlNifEnv* env, idx_state *pState, 
	GEOSGeometry* geom, 
		char (*pGEOS_Fun_r)(
			GEOSContextHandle_t, 
			const GEOSPreparedGeometry*,
			const GEOSGeometry*
		), int exact);

int64_t 
hash(const unsigned char* szVal);

long
get_crs(char *crs);

RTError
set_property(ErlNifEnv* env, int propType, ERL_NIF_TERM term, 
														IndexPropertyH props);

int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
	ErlNifResourceFlags flags = 
				(ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
	ErlNifResourceType* res;
	char* pszCsMapDir;

	res = enif_open_resource_type(env, NULL, "index_type", idx_state_dtor,
																 flags, NULL);

	csMapMutex = enif_mutex_create((char*)"csMapMutex");

	if ((csMapMutex == NULL) || (res == NULL))
	{
		return -1;
	}
	else
	{
		assert(index_type == NULL);
		index_type = res;

		idx_atoms.ok = enif_make_atom(env, "ok");
		idx_atoms.error = enif_make_atom(env, "error");

		// initialise CS Map library to use environment variable
		// CS_MAP_DIR for data dictionaries
		pszCsMapDir = getenv("CS_MAP_DIR");
		if (pszCsMapDir == NULL)
		{
			// set default which is used in testing
			char* realPath = realpath("../priv/CsDict", 0);
			CS_altdr(realPath);
			if (realPath)
				free(realPath);
		}
		else
		{
			CS_altdr(pszCsMapDir);
		}

		CS_init(0);
	
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

	if (csMapMutex != NULL) {
        enif_mutex_destroy(csMapMutex);
    }

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

	if ((handle != NULL) && Index_IsValid(handle))
	{
		pState->index = handle;

		pState->geosCtx = initGEOS_r(notice, error);
		GEOS_setWKBByteOrder_r(pState->geosCtx, GEOS_WKB_XDR);

		result = enif_make_resource(env, pState);
		enif_release_resource(pState);

		return enif_make_tuple2(env, idx_atoms.ok, result);
	}
	else
	{
		char buf[MAXBUFLEN];
		char* pszErrorMsg = Error_GetLastErrorMsg();
		sprintf(buf, "Unable to make a valid index: %s", pszErrorMsg);
		free(pszErrorMsg);

		return enif_make_tuple2(env, idx_atoms.error, 
			enif_make_string(env, 
				buf, ERL_NIF_LATIN1));
	}
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
		doc_len = bin.size + 1;		
		pszDocId = (unsigned char*)malloc(doc_len);
		memcpy(pszDocId, bin.data, bin.size);
		pszDocId[doc_len - 1] = 0;
	}
	else
		return enif_make_tuple2(env, idx_atoms.error, 
				enif_make_string(env, 
				"Unable to parse Id", ERL_NIF_LATIN1));

	// get wkb data and calculate min and max from geom
	if (enif_inspect_binary(env, argv[2], &wkb) &&
		(geom = GEOSGeomFromWKB_buf_r(pState->geosCtx, 
			wkb.data, wkb.size)) != NULL)
	{
		double* mins;
		double* maxs;
		unsigned char* pData;

		dims = GEOSGeom_getCoordinateDimension_r(pState->geosCtx, geom);
		mins = (double*)malloc(dims * sizeof(double));
		maxs = (double*)malloc(dims * sizeof(double));
		get_min_max(pState->geosCtx, geom, mins, maxs, dims);

		// doc id is null terminated join wkb data and doc id together
		pData = (unsigned char*)malloc(wkb.size + doc_len);
		
		memcpy(pData, (unsigned char*)pszDocId, doc_len);
		memcpy(pData + doc_len, wkb.data, wkb.size);

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
			sprintf(buf, "unable to insert document %s into index", pszDocId);
			return enif_make_tuple2(env, idx_atoms.error, 
				enif_make_string(env, buf, ERL_NIF_LATIN1));
		}

		free(mins);
		free(maxs);
		free(pData);
		free(pszDocId);
		GEOSGeom_destroy_r(pState->geosCtx, geom);
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
	int min_dims, max_dims;
	uint64_t nResults = 0;

	if (!enif_get_resource(env, argv[0], index_type, (void **) &pState))
		return enif_make_badarg(env);

	if (!enif_get_tuple(env, argv[1], &min_dims, &min_tuple))
		return enif_make_tuple2(env, idx_atoms.error,
			enif_make_string(env, "Unable to parse min tuple", ERL_NIF_LATIN1));

	if (!enif_get_tuple(env, argv[2], &max_dims, &max_tuple))
		return enif_make_tuple2(env, idx_atoms.error, 
			enif_make_string(env, "Unable to parse max tuple", ERL_NIF_LATIN1));

	if (!(min_dims == max_dims))
		return enif_make_tuple2(env, idx_atoms.error,
			enif_make_string(env, "min and max tuple arity needs to be equal", 
			ERL_NIF_LATIN1));

	double mins[min_dims];
	double maxs[max_dims];

	if (get_min_max_tuple(env, mins, maxs, min_tuple, max_tuple, min_dims) 
																	!= RT_None)
		return enif_make_tuple2(env, idx_atoms.error, 
			enif_make_string(env, "error getting min and max values", 
				ERL_NIF_LATIN1));

	if (Index_Intersects_count(pState->index, mins, 
								maxs, min_dims, &nResults) != RT_None)
		return enif_make_tuple2(env, idx_atoms.error, 
			enif_make_string(env, "unable to execute query", ERL_NIF_LATIN1));

	return enif_make_tuple2(env, idx_atoms.ok,  enif_make_uint64(env, nResults));
}

ERL_NIF_TERM
index_spatial_function(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	idx_state *pState;
	GEOSGeometry* geom = NULL;
	int functCode = 0;

	if (!enif_get_resource(env, argv[0], index_type, (void **) &pState))
		return enif_make_badarg(env);

	// is this a bbox, wkt or radius query
	switch (argc) {
	case 5: 
		int cnt;
		const ERL_NIF_TERM* tuple;
		// output crs is optional and assumed to EPSG:4326 LL if not specified
		// either a wkt string or {lat, lon, radius(m), crs} tuple
		// calculate mbr and call geos function with exact argument
		// if a tuple then radius else wkt
		if (!enif_get_tuple(env, argv[1], &cnt, &tuple))
		{
			char szWkt[MAXBUFLEN];
			if (enif_get_string(env, argv[1], szWkt, MAXBUFLEN, 
									ERL_NIF_LATIN1) > 0)
			{
				GEOSWKTReader* wktRdr = GEOSWKTReader_create_r(pState->geosCtx);
				geom = GEOSWKTReader_read_r(pState->geosCtx, wktRdr, szWkt);
				GEOSWKTReader_destroy_r(pState->geosCtx, wktRdr);
			}			
		}
		else
		{
			// radius, make a pt and buffer, radius is in metres
			if (cnt == 3)
			{
				double dist;
				char szDbCrs[MAXBUFLEN];
				long EpsgCode;
			 	char csSrcDefn[MAXBUFLEN];
				const char* csKeyName = "LL";
				double xyz[3];
				GEOSGeometry* pt;

				if  ((get_number(env, tuple, 0, &xyz[0]) == RT_None)
					&& (get_number(env, tuple, 1, &xyz[1]) == RT_None)
					&& (get_number(env, tuple, 2, &dist)) == RT_None)
				{	
					xyz[2] = 0.0;

					if (
						enif_get_string(env, argv[3], szDbCrs, 
							MAXBUFLEN, ERL_NIF_LATIN1) > 0)
					{
						// do nothing we have the full crs defn.
					}
					else
						strcpy(szDbCrs, "urn:ogc:def:crs:EPSG::4326");

					// only support EPSG
					// TODO add a special case for MGRS
					enif_mutex_lock(csMapMutex);
					EpsgCode = get_crs(szDbCrs);
					strcpy(csSrcDefn, CSepsg2adskCS(EpsgCode));

					// convert the coordinates
					if (CS_cnvrt(csSrcDefn, csKeyName, xyz) == 0)
					{
						double eRadius; // metres
						double eSq;
						char csEllipsoid[MAXBUFLEN];
						
						if ((CS_getEllipsoidOf(csKeyName, csEllipsoid, MAXBUFLEN) == 0)
							&& (CS_getElValues(csEllipsoid, &eRadius, &eSq) == 0))
						{
							double xyz_result[3];	

							// use the convenience method,
							// azimuth is degrees from north
	 						if (CS_azddll(eRadius, eSq, xyz, 90.0, dist,
	 							xyz_result) == 0)
	 						{
	 							// convert back to original cs
								if ((CS_cnvrt(csKeyName, csSrcDefn, xyz_result) == 0)
									&& CS_cnvrt(csKeyName, csSrcDefn, xyz) == 0)
								{
									GEOSCoordSequence* cs;
									GEOSBufferParams* bp = 
											GEOSBufferParams_create_r(pState->geosCtx);

									// create geometry which is a pt which is then buffered
									cs = GEOSCoordSeq_create_r(pState->geosCtx, 1, 2);
									GEOSCoordSeq_setX_r(pState->geosCtx,
										cs, 0, xyz[0]);
									GEOSCoordSeq_setY_r(pState->geosCtx,
										cs, 0, xyz[1]);
									// pt now owns cs
									pt = GEOSGeom_createPoint_r(pState->geosCtx,
												cs);

									if (pt != NULL)
									{					
										// buffer geom 
										geom = GEOSBufferWithParams_r(
											pState->geosCtx,
											pt, 
											bp,
											fabs(xyz[0] - xyz_result[0])
										);
									}
									else
										GEOSGeom_destroy_r(pState->geosCtx, pt);

									GEOSBufferParams_destroy_r(pState->geosCtx, bp);
								}
	 						}
	 					}
					}
					enif_mutex_unlock(csMapMutex);
				}
			}
		}

		enif_get_int(env, argv[4], &functCode);
		break;
	case 6:
	    // min max bbox query with exact intersection of index geometry
		const ERL_NIF_TERM* min_tuple;
		const ERL_NIF_TERM* max_tuple;
		int min_dims, max_dims;
		GEOSCoordSequence* cs;
		GEOSGeometry* ls;

		enif_get_tuple(env, argv[1], &min_dims, &min_tuple);
		enif_get_tuple(env, argv[2], &max_dims, &max_tuple);

		if (min_dims == max_dims)
		{
			double mins[min_dims];
			double maxs[max_dims];

			if (get_min_max_tuple(env, mins, maxs, min_tuple,
													max_tuple, min_dims) 
													== RT_None)
			{
				// make a bbox from the coordinates and call intersects
				cs = GEOSCoordSeq_create_r(pState->geosCtx, 2, min_dims);
				for (int i = 0; i < min_dims; i++)
				{
					GEOSCoordSeq_setOrdinate_r(pState->geosCtx,
						cs, 0, i, mins[i]);
					GEOSCoordSeq_setOrdinate_r(pState->geosCtx,
						cs, 1, i, maxs[i]);
				}

				// create an envelope of the min / max linestring
				// coordinates are now owned by linestring
				ls = GEOSGeom_createLineString_r(pState->geosCtx, cs);
				geom = GEOSEnvelope_r(pState->geosCtx, ls);
				GEOSGeom_destroy_r(pState->geosCtx, ls);
			}
		}

		enif_get_int(env, argv[3], &functCode);
		break;
	default: 
		// return
		return enif_make_tuple2(env, idx_atoms.error,
			enif_make_string(env, "unknown spatial function",
			ERL_NIF_LATIN1));
	}

	if (geom != NULL)
	{
		// switch function call
		ERL_NIF_TERM result;
		// all geometry operations are in the coordinate system of the DB
		 switch (functCode)
		{
			case 0:
			{
				// INTERSECTS_MBR
				result = spatial_function(env, pState, geom, &GEOSPreparedIntersects_r, 0);
				break;
			}
			case 1:
			{
				// INTERSECTS 
				result = spatial_function(env, pState, geom, &GEOSPreparedIntersects_r, 1);
				break;
			}
			case 2: 
			{
				// CONTAINS
				result = spatial_function(env, pState, geom, &GEOSPreparedContains_r, 1);
				break;
			}
			case 3: 
			{
				// CONTAINS PROPERLY
				result = spatial_function(env, pState, geom, &GEOSPreparedContainsProperly_r, 1);
				break;
			}
			case 4:
			{
				// COVERED BY
				result = spatial_function(env, pState, geom, &GEOSPreparedCoveredBy_r, 1);
				break;
			}
			case 5:
			{
				// COVERS
				result = spatial_function(env, pState, geom, &GEOSPreparedCovers_r, 1);
				break;
			}
			case 6:
			{
				// CROSSES
				result = spatial_function(env, pState, geom, &GEOSPreparedCrosses_r, 1);
				break;
			}
			case 7:
			{
				// DISJOINT
				result = spatial_function(env, pState, geom, &GEOSPreparedDisjoint_r, 1);
				break;
			}
			case 8:
			{
				// OVERLAPS
				result = spatial_function(env, pState, geom, &GEOSPreparedOverlaps_r, 1);
				break;
			}
			case 9:
			{
				// TOUCHES
				result = spatial_function(env, pState, geom, &GEOSPreparedTouches_r, 1);
				break;
			}
			case 10:
			{
				// WITHIN
				result = spatial_function(env, pState, geom, &GEOSPreparedWithin_r, 1);
				break;
			}
			default:
				GEOSGeom_destroy_r(pState->geosCtx, geom);
				return enif_make_tuple2(env, idx_atoms.error,
				enif_make_string(env, "unknown spatial function",
				ERL_NIF_LATIN1));
		}

		GEOSGeom_destroy_r(pState->geosCtx, geom);

		return result;
	}
	else
	{
	    // check CsMap
		char errMsg[MAXBUFLEN];
	    CS_errmsg(errMsg, MAXBUFLEN);

		return enif_make_tuple2(env, idx_atoms.error,
			enif_make_string(env, errMsg,
			ERL_NIF_LATIN1));
	}

}

ERL_NIF_TERM 
index_bounds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	idx_state *pState;
	uint32_t dims;
	double* pMins;
	double* pMaxs;
	ERL_NIF_TERM* pErlMins;
	ERL_NIF_TERM* pErlMaxs;
	ERL_NIF_TERM minTuple, maxTuple;

	if (!enif_get_resource(env, argv[0], index_type, (void **) &pState))
		return enif_make_badarg(env);

	Index_GetBounds(pState->index, &pMins, &pMaxs, &dims);

	pErlMins = (ERL_NIF_TERM*)malloc(dims * sizeof(ERL_NIF_TERM));
	pErlMaxs = (ERL_NIF_TERM*)malloc(dims * sizeof(ERL_NIF_TERM));

	for (uint32_t i = 0; i < dims; i++)
	{
		pErlMins[i] = enif_make_double(env, pMins[i]);
		pErlMaxs[i] = enif_make_double(env, pMaxs[i]);
	}

	minTuple = enif_make_tuple_from_array(env, pErlMins, dims);
	maxTuple = enif_make_tuple_from_array(env, pErlMaxs, dims);
	ERL_NIF_TERM lst = enif_make_list2(env, minTuple, maxTuple);

	free(pMins);
	free(pMaxs);
	free(pErlMins);
	free(pErlMaxs);

	return enif_make_tuple2(env, idx_atoms.ok, lst);
}

ERL_NIF_TERM
index_destroy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	idx_state *pState;
	if (!enif_get_resource(env, argv[0], index_type, (void **) &pState))
		return enif_make_badarg(env);

	Index_Destroy(pState->index);

	return idx_atoms.ok;
}

ERL_NIF_TERM
index_delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

	// get wkb data and calculate min and max from geom
	if (enif_inspect_binary(env, argv[2], &wkb) &&
		(geom = GEOSGeomFromWKB_buf_r(pState->geosCtx, 
			wkb.data, wkb.size)) != NULL)
	{
		double* mins;
		double* maxs;

		dims = GEOSGeom_getCoordinateDimension_r(pState->geosCtx, geom);
		mins = (double*)malloc(dims * sizeof(double));
		maxs = (double*)malloc(dims * sizeof(double));

		get_min_max(pState->geosCtx, geom, mins, maxs, dims);

		if (Index_DeleteData(pState->index, hash(pszDocId), mins,
				maxs, dims) != RT_None)
		{
			char buf[MAXBUFLEN];

			free(mins);
			free(maxs);
			free(pszDocId);
			GEOSGeom_destroy_r(pState->geosCtx, geom);

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
index_get_resultset_limit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	idx_state *pState;
	if (!enif_get_resource(env, argv[0], index_type, (void **) &pState))
		return enif_make_badarg(env);
	return enif_make_uint64(env, Index_GetResultSetLimit(pState->index));
}

ERL_NIF_TERM
index_set_resultset_limit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	idx_state *pState;
	uint64_t nResultLimit;
	if (!enif_get_resource(env, argv[0], index_type, (void **) &pState))
		return enif_make_badarg(env);
	
	if (!enif_get_uint64(env, argv[1], (ErlNifUInt64*) &nResultLimit))
		return enif_make_tuple(env, idx_atoms.error,
			enif_make_string(env, "Unable to parse resultset limit", ERL_NIF_LATIN1));

	if (Index_SetResultSetLimit(pState->index, nResultLimit) != RT_None)
	{
		return enif_make_tuple(env, idx_atoms.error,
			enif_make_string(env, "Unable to set resultset limit", ERL_NIF_LATIN1));
 
	}

	return idx_atoms.ok;
}

ERL_NIF_TERM
index_get_resultset_offset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	idx_state *pState;
	if (!enif_get_resource(env, argv[0], index_type, (void **) &pState))
		return enif_make_badarg(env);
	return enif_make_uint64(env, Index_GetResultSetOffset(pState->index));
}

ERL_NIF_TERM
index_set_resultset_offset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	idx_state *pState;
	uint64_t nResultLimit;
	if (!enif_get_resource(env, argv[0], index_type, (void **) &pState))
		return enif_make_badarg(env);
	
	if (!enif_get_uint64(env, argv[1], (ErlNifUInt64*) &nResultLimit))
		return enif_make_tuple(env, idx_atoms.error,
			enif_make_string(env, "Unable to parse resultset limit", ERL_NIF_LATIN1));

	if (Index_SetResultSetOffset(pState->index, nResultLimit) != RT_None)
	{
		return enif_make_tuple(env, idx_atoms.error,
			enif_make_string(env, "Unable to set resultset limit", ERL_NIF_LATIN1));
 
	}

	return idx_atoms.ok;
}


ERL_NIF_TERM 
index_flush(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	idx_state *pState;
	if (!enif_get_resource(env, argv[0], index_type, (void **) &pState))
		return enif_make_badarg(env);

	Index_Flush(pState->index);
	return idx_atoms.ok;
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
		case ResultSetLimit:
			uint64_t l;
			if (enif_get_uint64(env, term, (ErlNifUInt64*) &l))
			{
				IndexProperty_SetResultSetLimit(props, l);
			}
			else result = RT_Failure;
			break;
		default:
			result = RT_Failure;
	}

	return result;
}

int
get_min_max(GEOSContextHandle_t geosCtx, const GEOSGeometry* geom,
	double* const mins, double* const maxs, int dims)
{
	int n, type; 
	const GEOSCoordSequence* cs;
	const GEOSGeometry* env;
	const GEOSGeometry* g;

	if ((GEOSisValid_r(geosCtx, geom) == 1) && 
			!(GEOSisEmpty_r(geosCtx, geom)))
	{
		env = GEOSEnvelope_r(geosCtx, geom);
		type = GEOSGeomTypeId_r(geosCtx, env);
		switch(type)
		{
		case GEOS_POINT:
			cs = GEOSGeom_getCoordSeq_r(geosCtx, geom);
			return get_min_max_seq(geosCtx, cs, mins, maxs, dims, 1);
		default:
			g = GEOSGetExteriorRing_r(geosCtx, env);
			cs = GEOSGeom_getCoordSeq_r(geosCtx, g);
			n = GEOSGetNumCoordinates_r(geosCtx, g);
			return get_min_max_seq(geosCtx, cs, mins, maxs, dims, n);
		}
	}
	else 
		return RT_Failure;
}

int
get_min_max_seq(GEOSContextHandle_t geosCtx, const GEOSCoordSequence* cs, 
	double* const mins, double* const maxs, int dims, int n)
{
	int result = RT_None;
	for (int i = 0; i < dims; i++)
	{
		double v;

		if (GEOSCoordSeq_getOrdinate_r(geosCtx, cs, 0, i, &v))
		{
			mins[i] = v;
			maxs[i] = v;

			for (int j = 1; j < n; j++)
			{
				if (GEOSCoordSeq_getOrdinate_r(geosCtx, cs, j, i, &v))
				{
					if (v > maxs[i])
						maxs[i] = v;

					if (v < mins[i])
						mins[i] = v;
				}
				else
				{
					result = RT_Failure;
					break;
				}
			}
		}
		else
		{
			result = RT_Failure;
			break;
		}
	}
	return result;
}

ERL_NIF_TERM spatial_function(ErlNifEnv* env, idx_state *pState, 
	GEOSGeometry* geom,
	char (*pGEOS_Fun_r)(
			GEOSContextHandle_t, 
			const GEOSPreparedGeometry*,
			const GEOSGeometry*
		), int exact)
{
	ERL_NIF_TERM resultList;
	const GEOSPreparedGeometry* pg = NULL;
	uint64_t nResults;
	IndexItemH* items;
	int dims;
	double* mins;
	double* maxs;

	if (exact)
		pg = GEOSPrepare_r(pState->geosCtx, geom);
	
	// calculate mins and maxs from input geometry
	dims = GEOSGeom_getCoordinateDimension_r(pState->geosCtx, geom);
	mins = (double*)malloc(dims * sizeof(double));
	maxs = (double*)malloc(dims * sizeof(double));
	get_min_max(pState->geosCtx, geom, mins, maxs, dims);

	if (Index_Intersects_obj(pState->index, mins, maxs,
							 dims, &items, &nResults) != RT_None)
	{
		free(mins);
		free(maxs);
		if (exact)
			GEOSPreparedGeom_destroy_r(pState->geosCtx, pg);

		return enif_make_tuple2(env, idx_atoms.error, 
		  enif_make_string(env, "unable to execute query", ERL_NIF_LATIN1));
	}
	free(mins);
	free(maxs);

	resultList = enif_make_list(env, 0);

	for (uint64_t i = 0; i < nResults; i++)
	{
		unsigned char* data = NULL;
		uint64_t len = 0;
		IndexItemH item = items[i];
		GEOSGeometry* wkb = NULL;

		if (IndexItem_GetData(item, (uint8_t **)&data, &len) == RT_None)
		{
			ErlNifBinary bin;
			int doc_len;

			// data is a NULL terminated string followed by WKB
			doc_len = strlen((char*)data) + 1;

			if (enif_alloc_binary(doc_len - 1, &bin))
			{
				if (exact)
				{
					// parse item WKB and test function exactly
					wkb = GEOSGeomFromWKB_buf_r(pState->geosCtx, 
												data + doc_len,
												len - doc_len);
					if (wkb != NULL)
					{	
						if ((*pGEOS_Fun_r)(pState->geosCtx,
										pg, wkb))
						{
							memcpy(bin.data, data, doc_len - 1);
							ERL_NIF_TERM head = enif_make_binary(env, &bin);
							resultList = enif_make_list_cell(env, head,
															resultList);
						}
						GEOSGeom_destroy_r(pState->geosCtx, wkb);
					}
					else
					{
						char buf[MAXBUFLEN];
						char doc[doc_len];
						memcpy(doc, data, doc_len);
						doc[doc_len - 1] = 0;

						sprintf(buf, "unable to parse document %s geometry",
												 doc);

						free(data);
						IndexItem_Destroy(item);

						if (exact)
							GEOSPreparedGeom_destroy_r(pState->geosCtx, pg);

						return enif_make_tuple2(env, idx_atoms.error, 
							enif_make_string(env, buf, ERL_NIF_LATIN1));
					}
				}
				else
				{
					memcpy(bin.data, data, doc_len - 1);
					ERL_NIF_TERM head = enif_make_binary(env, &bin);
					resultList = enif_make_list_cell(env, head, resultList);
				}
			}
			else
			{
				char buf[MAXBUFLEN];
				char doc[doc_len];
				memcpy(doc, data, doc_len);
				doc[doc_len - 1] = 0;
				sprintf(buf, "unable to assign doc id %s to erlang term",
										 doc);

				free(data);
				IndexItem_Destroy(item);

				if (exact)
					GEOSPreparedGeom_destroy_r(pState->geosCtx, pg);

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
	} // end for loop

	if (exact)
		GEOSPreparedGeom_destroy_r(pState->geosCtx, pg);

	return enif_make_tuple2(env, idx_atoms.ok, resultList);
}

int 
get_number(ErlNifEnv* env, const ERL_NIF_TERM* tuple, int pos, double* v)
{
	if (!enif_get_double(env, tuple[pos], v))
	{
		int v1;
		if (enif_get_int(env, tuple[pos], &v1))
			*v = (double)v1;
		else
			return RT_Failure;
	}
	return RT_None;
}

int
get_min_max_tuple(ErlNifEnv* env, double* mins, double* maxs,
	 const ERL_NIF_TERM* min_tuple, const ERL_NIF_TERM* max_tuple, int dims)
{
	for (int i = 0; i < dims; i++)
	{
		double d1, d2;

		if (get_number(env, min_tuple, i, &d1) != RT_None)
			return RT_Failure;

		mins[i] = d1;

		if (get_number(env, max_tuple, i, &d2) != RT_None)
			return RT_Failure;

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

long
get_crs(char *crs)  {
    char* ptr = strrchr(crs,':');
    if( ptr != NULL)
        ptr++;
    return atol(ptr);
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
