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

// break with c api for ellipse support
#include "geos/geom/PrecisionModel.h"
#include "geos/geom/GeometryFactory.h"
#include "geos/util/GeometricShapeFactory.h"
#include "geos/geom/Coordinate.h"
#include "geos/geom/Polygon.h"
#include <geos/io/WKTWriter.h>

#include <sstream>
#include <iomanip>

// csmap - http://trac.osgeo.org/csmap/
#include "cs_map.h"
#include "csNameMapperSupport.h"

#define MAXBUFLEN	1024

#define DEFAULT_CRS "urn:ogc:def:crs:EPSG::4326"

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
    fflush(stdout);
}

void
error(const char *fmt, ...) {
	va_list ap;
  fprintf( stdout, "ERROR: ");
	va_start (ap, fmt);
  vfprintf( stdout, fmt, ap);
  va_end(ap);
  fprintf( stdout, "\n" );
	fflush(stdout);
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

GEOSGeometry*
reproject_geom(GEOSContextHandle_t geosCtx, const GEOSGeometry* geom, long src, long target);

ERL_NIF_TERM
spatial_function(ErlNifEnv* env, idx_state *pState,
	GEOSGeometry* geom,
		char (*pGEOS_Fun_r)(
			GEOSContextHandle_t,
			const GEOSPreparedGeometry*,
			const GEOSGeometry*
		));

ERL_NIF_TERM
spatial_mbr(ErlNifEnv* env, idx_state *pState, int dims, double* mins, double* maxs, bool nearest);

ERL_NIF_TERM
spatial_tmbr(ErlNifEnv* env, idx_state *pState, int dims, double* mins, double* maxs, double tStart, double tEnd, bool nearest);

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
get_centre(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary wkb;
	ERL_NIF_TERM result;
	GEOSContextHandle_t geosCtx;
	GEOSGeometry* geom = NULL;
	GEOSGeometry* ct = NULL;

    // initialise geos on every get centre call
	geosCtx = initGEOS_r(notice, error);
	GEOS_setWKBByteOrder_r(geosCtx, GEOS_WKB_XDR);

	if (enif_inspect_binary(env, argv[0], &wkb) &&
		(geom = GEOSGeomFromWKB_buf_r(geosCtx,
			wkb.data, wkb.size)) != NULL)
	{
		if ((ct = GEOSGetCentroid_r(geosCtx, geom)) != NULL)
		{
			double x, y;
			const GEOSCoordSequence* coords;

			coords = GEOSGeom_getCoordSeq_r(geosCtx, ct);

			if (GEOSCoordSeq_getX_r(geosCtx, coords, 0, &x) &&
				GEOSCoordSeq_getY_r(geosCtx, coords, 0, &y))
			{
				result = enif_make_tuple2(env, idx_atoms.ok,
					enif_make_tuple2(env, enif_make_double(env, x), enif_make_double(env, y)));
			}
			else
				result = enif_make_tuple2(env, idx_atoms.error,
					enif_make_string(env, "Unable to get centroid coordinates",
						ERL_NIF_LATIN1));
		}
		else
			result = enif_make_tuple2(env, idx_atoms.error,
				enif_make_string(env, "Unable to calculate centroid",
					ERL_NIF_LATIN1));
	}
	else
		result = enif_make_tuple2(env, idx_atoms.error,
					enif_make_string(env, "Unable to parse geometry",
						ERL_NIF_LATIN1));

	if (geom != NULL)
		GEOSGeom_destroy_r(geosCtx, geom);

	if (ct != NULL)
		GEOSGeom_destroy_r(geosCtx, ct);

	finishGEOS_r(geosCtx);

	return result;
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
		unsigned char* pData;

		// dims is dependent on the index used
		IndexPropertyH props = Index_GetProperties(pState->index);
		dims = (int)IndexProperty_GetDimension(props);

		double mins[dims];
		double maxs[dims];
		get_min_max(pState->geosCtx, geom, mins, maxs, dims);

		// doc id is null terminated join wkb data and doc id together
		pData = (unsigned char*)malloc(wkb.size + doc_len);

		memcpy(pData, (unsigned char*)pszDocId, doc_len);
		memcpy(pData + doc_len, wkb.data, wkb.size);

		if (argc == 8)
		{
			// temporal insert
			const ERL_NIF_TERM* minv_tuple;
			const ERL_NIF_TERM* maxv_tuple;
			bool bSuccess = TRUE;
			double tStart, tEnd;

			double minsV[dims];
			double maxsV[dims];

			// temporal data
			if (!enif_get_tuple(env, argv[3], &dims, &minv_tuple))
			{
				bSuccess = FALSE;
			}

			if (!enif_get_tuple(env, argv[4], &dims, &maxv_tuple))
			{
				bSuccess = FALSE;
			}

			if (get_min_max_tuple(env, minsV, maxsV, minv_tuple, maxv_tuple, dims)
																!= RT_None)
			{
				bSuccess = FALSE;
			}

			if (!enif_get_double(env, argv[5], &tStart))
			{
				int tS;
				if (!enif_get_int(env, argv[5], &tS))
				{
					bSuccess = FALSE;
				}
				else
					tStart = tS;
			}

			if (!enif_get_double(env, argv[6], &tEnd))
			{
				int tE;
				if (!enif_get_int(env, argv[6], &tE))
				{
					bSuccess = FALSE;
				}
				else
					tEnd = tE;
			}

			if (bSuccess)
			{
				if (Index_InsertTPData(pState->index,
					hash(pszDocId), mins, maxs, minsV, maxsV, tStart, tEnd, dims,
					(uint8_t *)pData, wkb.size + doc_len) != RT_None)
				{
					char buf[MAXBUFLEN];

					free(pData);
					free(pszDocId);
					GEOSGeom_destroy_r(pState->geosCtx, geom);
					char* pszErrorMsg = Error_GetLastErrorMsg();
					sprintf(buf, "unable to insert document %s into index: %s", pszDocId, pszErrorMsg);
					free(pszErrorMsg);
					return enif_make_tuple2(env, idx_atoms.error,
						enif_make_string(env, buf, ERL_NIF_LATIN1));
				}
			}
			else
			{
				free(pData);
				free(pszDocId);
				GEOSGeom_destroy_r(pState->geosCtx, geom);
				return enif_make_tuple2(env, idx_atoms.error,
						enif_make_string(env,
						"Unable to parse velocity bounding rectangle", ERL_NIF_LATIN1));
			}
		}
		else
		{
			if (Index_InsertData(pState->index,
				hash(pszDocId), mins, maxs, dims,
				(uint8_t *)pData, wkb.size + doc_len) != RT_None)
			{
				char buf[MAXBUFLEN];

				free(pData);
				free(pszDocId);
				GEOSGeom_destroy_r(pState->geosCtx, geom);
				sprintf(buf, "unable to insert document %s into index", pszDocId);
				return enif_make_tuple2(env, idx_atoms.error,
					enif_make_string(env, buf, ERL_NIF_LATIN1));
			}
		}

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
	const ERL_NIF_TERM* minv_tuple;
	const ERL_NIF_TERM* maxv_tuple;
	double tStart, tEnd;
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

	if (argc == 7)
	{
		if (!enif_get_tuple(env, argv[3], &min_dims, &minv_tuple))
			return enif_make_tuple2(env, idx_atoms.error,
				enif_make_string(env, "Unable to parse low velocity tuple", ERL_NIF_LATIN1));

		if (!enif_get_tuple(env, argv[4], &max_dims, &maxv_tuple))
			return enif_make_tuple2(env, idx_atoms.error,
				enif_make_string(env, "Unable to parse high velocity tuple", ERL_NIF_LATIN1));

		if (!enif_get_double(env, argv[5], &tStart))
		{
			int tS;
			if (!enif_get_int(env, argv[5], &tS))
			{
				return enif_make_tuple2(env, idx_atoms.error,
					enif_make_string(env, "Unable to parse start time", ERL_NIF_LATIN1));
			}
			else
				tStart = tS;
		}

		if (!enif_get_double(env, argv[6], &tEnd))
		{
			int tE;
			if (!enif_get_int(env, argv[6], &tE))
			{
				return enif_make_tuple2(env, idx_atoms.error,
					enif_make_string(env, "Unable to parse end time", ERL_NIF_LATIN1));
			}
			tEnd = tE;
		}
	}

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

	if (argc == 7)
	{
		double vmins[min_dims];
		double vmaxs[max_dims];

		if (get_min_max_tuple(env, vmins, vmaxs, minv_tuple, maxv_tuple, min_dims)
																		!= RT_None)
			return enif_make_tuple2(env, idx_atoms.error,
				enif_make_string(env, "error getting low and high velocity values",
					ERL_NIF_LATIN1));

		if (Index_TPIntersects_count(pState->index, mins,
									maxs, vmins, vmaxs, tStart, tEnd, min_dims, &nResults) != RT_None)
		{
				char buf[MAXBUFLEN];
				char* pszErrorMsg = Error_GetLastErrorMsg();
				sprintf(buf, "Unable to execute temporal query: %s", pszErrorMsg);
				free(pszErrorMsg);

				return enif_make_tuple2(env, idx_atoms.error,
					enif_make_string(env,
						buf, ERL_NIF_LATIN1));
		}
	}
	else
	{
		if (Index_Intersects_count(pState->index, mins,
									maxs, min_dims, &nResults) != RT_None)
		{
				char buf[MAXBUFLEN];
				char* pszErrorMsg = Error_GetLastErrorMsg();
				sprintf(buf, "Unable to execute query: %s", pszErrorMsg);
				free(pszErrorMsg);

				return enif_make_tuple2(env, idx_atoms.error,
					enif_make_string(env,
						buf, ERL_NIF_LATIN1));
		}
	}

	return enif_make_tuple2(env, idx_atoms.ok,  enif_make_uint64(env, nResults));
}

ERL_NIF_TERM
index_spatial_function(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	idx_state *pState;
	GEOSGeometry* geom = NULL;
	double* mins = NULL;
	double* maxs = NULL;
	double tStart = 0.0;
	double tEnd = 0.0;
	uint32_t dims;

	int functCode = 0;
	char szDbCrs[MAXBUFLEN];
	char szReqCrs[MAXBUFLEN];

	if (!enif_get_resource(env, argv[0], index_type, (void **) &pState))
		return enif_make_badarg(env);

	// is this a bbox, wkt or radius query
	switch (argc) {
	case 5:
	{
		int cnt;
		const ERL_NIF_TERM* tuple;

		if (enif_get_string(env, argv[3], szDbCrs,
			MAXBUFLEN, ERL_NIF_LATIN1) > 0)
		{
			// do nothing
		}
		else
			strcpy(szDbCrs, DEFAULT_CRS);

		// either a wkt string or {lat, lon, radius(m)} tuple
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

				// try to reproject the request geometry if possible
				if (enif_get_string(env, argv[2], szReqCrs,
					MAXBUFLEN, ERL_NIF_LATIN1) > 0)
				{
						GEOSGeometry* pProjGeom =
							reproject_geom(pState->geosCtx,
								geom, get_crs(szReqCrs), get_crs(szDbCrs));
						if (pProjGeom != NULL)
						{
							GEOSGeom_destroy_r(pState->geosCtx, geom);
							geom = pProjGeom;
						}
						else
						{
							// failure
							GEOSGeom_destroy_r(pState->geosCtx, geom);
							geom = NULL;
						}
				}

				GEOSWKTReader_destroy_r(pState->geosCtx, wktRdr);
			}
		}
		else
		{
			// radius, make a pt and buffer
			switch (cnt)
			{
				case 3: // circle
				{
					double dist;

					long EpsgCode, reqEpsgCode;
					char csSrcDefn[MAXBUFLEN];
					const char* csKeyName = "LL";
					double xyz[3];
					GEOSGeometry* pt;
					int bReprojected = 1;

					if  ((get_number(env, tuple, 0, &xyz[0]) == RT_None)
						&& (get_number(env, tuple, 1, &xyz[1]) == RT_None)
						&& (get_number(env, tuple, 2, &dist)) == RT_None)
					{
						xyz[2] = 0.0;

						if (enif_get_string(env, argv[3], szDbCrs,
							MAXBUFLEN, ERL_NIF_LATIN1) > 0)
						{
							// do nothing
						}
						else
							strcpy(szDbCrs, DEFAULT_CRS);

						// only support EPSG
						enif_mutex_lock(csMapMutex);

						// if there is a request crs we need to convert this
						// xyz point to the src crs
						EpsgCode = get_crs(szDbCrs);
						strcpy(csSrcDefn, CSepsg2adskCS(EpsgCode));

						if (enif_get_string(env, argv[2], szReqCrs,
							MAXBUFLEN, ERL_NIF_LATIN1) > 0)
						{
							reqEpsgCode = get_crs(szReqCrs);

							if (CS_cnvrt(CSepsg2adskCS(reqEpsgCode), csSrcDefn, xyz) != 0)
							{
								bReprojected = 0;
							}
						}

						// convert the coordinates
						if ((bReprojected == 1) && (CS_cnvrt(csSrcDefn, csKeyName, xyz) == 0))
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
					break;
				}
				case 4: // ellipse, breaks geos C API
				{
					double x_range, y_range;
					long EpsgCode, reqEpsgCode;
					char csSrcDefn[MAXBUFLEN];
					const char* csKeyName = "LL";
					double xyz[3];
					int bReprojected = 1;

					geos::geom::PrecisionModel *pm = new geos::geom::PrecisionModel();
					geos::geom::GeometryFactory *global_factory = new  geos::geom::GeometryFactory(pm);;
					geos::util::GeometricShapeFactory shapefactory(global_factory);

					// We do not need PrecisionMode object anymore, it has
					// been copied to global_factory private storage
					delete pm;

					if  ((get_number(env, tuple, 0, &xyz[0]) == RT_None)
						&& (get_number(env, tuple, 1, &xyz[1]) == RT_None)
						&& (get_number(env, tuple, 2, &x_range) == RT_None)
						&& (get_number(env, tuple, 2, &y_range)) == RT_None)
					{
						xyz[2] = 0.0;

						if (enif_get_string(env, argv[3], szDbCrs,
							MAXBUFLEN, ERL_NIF_LATIN1) > 0)
						{
							// do nothing
						}
						else
							strcpy(szDbCrs, DEFAULT_CRS);

						// only support EPSG
						enif_mutex_lock(csMapMutex);

						// if there is a request crs we need to convert this
						// xyz point to the src crs
						EpsgCode = get_crs(szDbCrs);
						strcpy(csSrcDefn, CSepsg2adskCS(EpsgCode));

						// convert x, y to db crs
						if (enif_get_string(env, argv[2], szReqCrs,
							MAXBUFLEN, ERL_NIF_LATIN1) > 0)
						{
							reqEpsgCode = get_crs(szReqCrs);

							if (CS_cnvrt(CSepsg2adskCS(reqEpsgCode), csSrcDefn, xyz) != 0)
							{
								bReprojected = 0;
							}
						}

						// convert the coordinates
						if ((bReprojected == 1) && (CS_cnvrt(csSrcDefn, csKeyName, xyz) == 0))
						{
							// break with GEOS C API and create an Arc Polygon, not as accurate as radius, not peformed on ellipsoid
							// workaround to write to wkb and then read back with c api.
							// TODO use WKB not WKT
							geos::geom::Polygon* poly;
							std::string s;
							geos::io::WKTWriter *wktWriter = new geos::io::WKTWriter();

							// calculate x_range and y_range on the earth surface
							double eRadius; // metres
							double eSq;
							char csEllipsoid[MAXBUFLEN];

							if ((CS_getEllipsoidOf(csKeyName, csEllipsoid, MAXBUFLEN) == 0)
								&& (CS_getElValues(csEllipsoid, &eRadius, &eSq) == 0))
							{
								// holds the distance along x,y axis for the ellipse
								double xyz_result_x[3];
								double xyz_result_y[3];

								// use the convenience method,
								// azimuth is degrees from north
								if ((CS_azddll(eRadius, eSq, xyz, 90.0, x_range,
									xyz_result_x) == 0) &&
									(CS_azddll(eRadius, eSq, xyz, 0.0, y_range, xyz_result_y) == 0))
								{
									// convert back to original cs
									if ((CS_cnvrt(csKeyName, csSrcDefn, xyz_result_x) == 0)
										&& (CS_cnvrt(csKeyName, csSrcDefn, xyz_result_y) == 0) &&
										(CS_cnvrt(csKeyName, csSrcDefn, xyz) == 0))
									{
										x_range = fabs(xyz[0] - xyz_result_x[0]);
										y_range = fabs(xyz[1] - xyz_result_y[1]);

										// geos c
										GEOSWKTReader* wktReader;
										shapefactory.setCentre(geos::geom::Coordinate(xyz[0], xyz[1]));
										shapefactory.setWidth(x_range);
										shapefactory.setHeight(y_range);
										poly = shapefactory.createCircle();

										// serialize to wkt
										wktWriter->setOutputDimension(3);
										s = wktWriter->write(poly);
										delete wktWriter;

										wktReader = GEOSWKTReader_create_r(pState->geosCtx);
										geom = GEOSWKTReader_read_r(pState->geosCtx, wktReader,
											s.c_str());

										GEOSWKTReader_destroy_r(pState->geosCtx, wktReader);
									}
								}
							}
						}

						enif_mutex_unlock(csMapMutex);
					}

					delete global_factory;
					break;
				}
				default:
				{
					geom = NULL;
				}
			}
		}

		enif_get_int(env, argv[4], &functCode);
		break;
	}
	case 8:
	{
		// note not using a break here intentionally
		if (argc == 8)
		{
			// get start and end time
			if (!enif_get_double(env, argv[6], &tStart))
			{
				int tS;
				enif_get_int(env, argv[6], &tS);
				tStart =tS;
			}

			if (!enif_get_double(env, argv[7], &tEnd))
			{
				int tE;
				enif_get_int(env, argv[7], &tE);
				tEnd = tE;
			}
		}
	}
	case 6:
	{
			// min max bbox query with intersection or nearest of index geometry
		const ERL_NIF_TERM* min_tuple;
		const ERL_NIF_TERM* max_tuple;
		int min_dims, max_dims;
		GEOSCoordSequence* cs;
		GEOSGeometry* ls;

		enif_get_tuple(env, argv[1], &min_dims, &min_tuple);
		enif_get_tuple(env, argv[2], &max_dims, &max_tuple);

		if (enif_get_string(env, argv[4], szDbCrs,
			MAXBUFLEN, ERL_NIF_LATIN1) > 0)
		{
			// do nothing
		}
		else
			strcpy(szDbCrs, DEFAULT_CRS);


		if (min_dims == max_dims)
		{
			enif_get_int(env, argv[5], &functCode);

			// dims is dependent on the index used
			IndexPropertyH props = Index_GetProperties(pState->index);
			dims = IndexProperty_GetDimension(props);

			mins = (double*)calloc(dims, sizeof(double));
			maxs = (double*)calloc(dims, sizeof(double));

			if (get_min_max_tuple(env, mins, maxs, min_tuple,
													max_tuple, min_dims)
													== RT_None)
			{
				if ((functCode != ST_INTERSECTS_MBR)
					&& (functCode != ST_NEAREST)
					&& (functCode != ST_TPINTERSECTS_MBR))
				{
					// geometry query
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

					// try to reproject the request geometry if possible
					if (enif_get_string(env, argv[3], szReqCrs,
						MAXBUFLEN, ERL_NIF_LATIN1) > 0)
					{
						GEOSGeometry* pProjGeom =
							reproject_geom(pState->geosCtx,
								geom, get_crs(szReqCrs), get_crs(szDbCrs));
						if (pProjGeom != NULL)
						{
							GEOSGeom_destroy_r(pState->geosCtx, geom);
							geom = pProjGeom;
						}
					}
					GEOSGeom_destroy_r(pState->geosCtx, ls);
				}
				else
				{
					if (enif_get_string(env, argv[3], szReqCrs,
						MAXBUFLEN, ERL_NIF_LATIN1) > 0)
					{
							// reproject mins/maxs array
							long src = get_crs(szReqCrs);
							long target = get_crs(szDbCrs);
							char csSrcDefn[MAXBUFLEN];
							char csTgtDefn[MAXBUFLEN];

							if (target != src)
							{
								// reproject coord seq,
								// only support EPSG
								enif_mutex_lock(csMapMutex);

								strcpy(csSrcDefn, CSepsg2adskCS(src));
								strcpy(csTgtDefn, CSepsg2adskCS(target));

								CS_cnvrt(csSrcDefn, csTgtDefn, mins);
								CS_cnvrt(csSrcDefn, csTgtDefn, maxs);

								enif_mutex_unlock(csMapMutex);
							}
					}
				}
			}
		}
		break;
	}
	default:
		// return
		return enif_make_tuple2(env, idx_atoms.error,
			enif_make_string(env, "unknown spatial function",
			ERL_NIF_LATIN1));
	}

	if ((geom != NULL) ||
		(functCode == ST_INTERSECTS_MBR) ||
		 (functCode == ST_NEAREST) ||
		 (functCode == ST_TPINTERSECTS_MBR))
	{
		// switch function call
		ERL_NIF_TERM result;

		// all geometry operations are in the coordinate system of the DB
		switch (functCode)
		{
			case ST_INTERSECTS_MBR:
			{
				result = spatial_mbr(env, pState, dims, mins, maxs, false);
				break;
			}
			case ST_TPINTERSECTS_MBR:
			{
				result = spatial_tmbr(env, pState, dims, mins, maxs, tStart, tEnd, false);
				break;
			}
			case ST_TPNEAREST:
			{
				result = spatial_tmbr(env, pState, dims, mins, maxs, tStart, tEnd, true);
				break;
			}
			case ST_INTERSECTS:
			{
				result = spatial_function(env, pState, geom, &GEOSPreparedIntersects_r);
				break;
			}
			case ST_CONTAINS:
			{
				result = spatial_function(env, pState, geom, &GEOSPreparedContains_r);
				break;
			}
			case ST_CONTAINS_PROPERLY:
			{
				result = spatial_function(env, pState, geom, &GEOSPreparedContainsProperly_r);
				break;
			}
			case ST_COVERED_BY:
			{
				result = spatial_function(env, pState, geom, &GEOSPreparedCoveredBy_r);
				break;
			}
			case ST_COVERS:
			{
				result = spatial_function(env, pState, geom, &GEOSPreparedCovers_r);
				break;
			}
			case ST_CROSSES:
			{
				result = spatial_function(env, pState, geom, &GEOSPreparedCrosses_r);
				break;
			}
			case ST_DISJOINT:
			{
				result = spatial_function(env, pState, geom, &GEOSPreparedDisjoint_r);
				break;
			}
			case ST_OVERLAPS:
			{
				result = spatial_function(env, pState, geom, &GEOSPreparedOverlaps_r);
				break;
			}
			case ST_TOUCHES:
			{
				result = spatial_function(env, pState, geom, &GEOSPreparedTouches_r);
				break;
			}
			case ST_WITHIN:
			{
				result = spatial_function(env, pState, geom, &GEOSPreparedWithin_r);
				break;
			}
			case ST_NEAREST:
			{
				result = spatial_mbr(env, pState, dims, mins, maxs, true);
				break;
			}
			default:
				GEOSGeom_destroy_r(pState->geosCtx, geom);
				return enif_make_tuple2(env, idx_atoms.error,
				enif_make_string(env, "unknown spatial function",
				ERL_NIF_LATIN1));
		}

		if (geom != NULL)
			GEOSGeom_destroy_r(pState->geosCtx, geom);

		if (mins)
			free(mins);

		if (maxs)
			free(maxs);

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

	if (pState->index)
		Index_Destroy(pState->index);

	pState->index = NULL;

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
		// dims is dependent on the index used
		IndexPropertyH props = Index_GetProperties(pState->index);
		dims = (int)IndexProperty_GetDimension(props);

		double mins[dims];
		double maxs[dims];

		get_min_max(pState->geosCtx, geom, mins, maxs, dims);

		if (argc == 8)
		{
			// temporal delete
			const ERL_NIF_TERM* minv_tuple;
			const ERL_NIF_TERM* maxv_tuple;
			bool bSuccess = TRUE;
			double tStart, tEnd;
			double minsV[dims];
			double maxsV[dims];

			// temporal data
			if (!enif_get_tuple(env, argv[3], &dims, &minv_tuple))
			{
				bSuccess = FALSE;
			}

			if (!enif_get_tuple(env, argv[4], &dims, &maxv_tuple))
			{
				bSuccess = FALSE;
			}

			if (get_min_max_tuple(env, minsV, maxsV, minv_tuple, maxv_tuple, dims)
																!= RT_None)
			{
				bSuccess = FALSE;
			}

			if (!enif_get_double(env, argv[5], &tStart))
			{
				int tS;
				if (!enif_get_int(env, argv[5], &tS))
				{
					bSuccess = FALSE;
				}
				else
					tStart = tS;
			}

			if (!enif_get_double(env, argv[6], &tEnd))
			{
				int tE;
				if (!enif_get_int(env, argv[6], &tE))
				{
					bSuccess = FALSE;
				}
				else
					tEnd = tE;
			}

			if (bSuccess)
			{
				if (Index_DeleteTPData(pState->index, hash(pszDocId), mins,
						maxs, minsV, maxsV, tStart, tEnd, dims) != RT_None)
				{
					char buf[MAXBUFLEN];
					char* pszErrorMsg = Error_GetLastErrorMsg();
					free(pszDocId);
					GEOSGeom_destroy_r(pState->geosCtx, geom);

					sprintf(buf, "Unable to delete document: %s : %s", pszDocId, pszErrorMsg);
					free(pszErrorMsg);

					return enif_make_tuple2(env, idx_atoms.error,
								enif_make_string(env, buf, ERL_NIF_LATIN1));
				}
			}
			else
			{
				free(pszDocId);
				GEOSGeom_destroy_r(pState->geosCtx, geom);
				return enif_make_tuple2(env, idx_atoms.error,
						enif_make_string(env,
						"Unable to parse velocity bounding rectangle", ERL_NIF_LATIN1));
			}
		}
		else
		{
			if (Index_DeleteData(pState->index, hash(pszDocId), mins,
					maxs, dims) != RT_None)
			{
				char buf[MAXBUFLEN];
				free(pszDocId);
				GEOSGeom_destroy_r(pState->geosCtx, geom);

				sprintf(buf, "unable to delete document %s from index", pszDocId);
				return enif_make_tuple2(env, idx_atoms.error,
							enif_make_string(env, buf, ERL_NIF_LATIN1));

			}
		}
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
index_get_resultset_limit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	idx_state *pState;
	if (!enif_get_resource(env, argv[0], index_type, (void **) &pState))
		return enif_make_badarg(env);
	return enif_make_int64(env, Index_GetResultSetLimit(pState->index));
}

ERL_NIF_TERM
index_set_resultset_limit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	idx_state *pState;
	int64_t nResultLimit;
	if (!enif_get_resource(env, argv[0], index_type, (void **) &pState))
		return enif_make_badarg(env);

	if (!enif_get_int64(env, argv[1], (ErlNifSInt64*) &nResultLimit))
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
	return enif_make_int64(env, Index_GetResultSetOffset(pState->index));
}

ERL_NIF_TERM
index_set_resultset_offset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	idx_state *pState;
	int64_t nResultLimit;
	if (!enif_get_resource(env, argv[0], index_type, (void **) &pState))
		return enif_make_badarg(env);

	if (!enif_get_int64(env, argv[1], (ErlNifSInt64*) &nResultLimit))
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
			int64_t l;
			if (enif_get_int64(env, term, (ErlNifSInt64*) &l))
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
GEOSGeometry*
reproject_geom(GEOSContextHandle_t geosCtx, const GEOSGeometry* geom, long src, long target)
{
	const GEOSCoordSequence* cs;
	GEOSCoordSequence* clone;
  char csSrcDefn[MAXBUFLEN];
  char csTgtDefn[MAXBUFLEN];
	double xyz[3];
	const GEOSGeometry* g;
	unsigned int nDims = 0;

	if (target != src)
	{
		int type;
		type = GEOSGeomTypeId_r(geosCtx, geom);
		// polygon or linestring
		switch(type)
		{
			case GEOS_POLYGON:
				g = GEOSGetExteriorRing_r(geosCtx, geom);
				cs = GEOSGeom_getCoordSeq_r(geosCtx, g);
				break;
			default:
				cs = GEOSGeom_getCoordSeq_r(geosCtx, geom);
		}

		// reproject coord seq,
		// only support EPSG
		enif_mutex_lock(csMapMutex);

		strcpy(csSrcDefn, CSepsg2adskCS(src));
		strcpy(csTgtDefn, CSepsg2adskCS(target));
		GEOSCoordSeq_getSize_r(geosCtx, cs, &nDims);
		clone = GEOSCoordSeq_clone_r(geosCtx, cs);

		// convert the coordinates
		for (int i = 0; i < nDims; i++)
		{
			GEOSCoordSeq_getX_r(geosCtx, cs, i, &xyz[0]);
			GEOSCoordSeq_getY_r(geosCtx, cs, i, &xyz[1]);
			GEOSCoordSeq_getZ_r(geosCtx, cs, i, &xyz[2]);

			if (CS_cnvrt(csSrcDefn, csTgtDefn, xyz) != 0)
			{
				enif_mutex_unlock(csMapMutex);
				return NULL;
			}

			GEOSCoordSeq_setX_r(geosCtx, clone, i, xyz[0]);
			GEOSCoordSeq_setY_r(geosCtx, clone, i, xyz[1]);
			GEOSCoordSeq_setZ_r(geosCtx, clone, i, xyz[2]);

		}

		enif_mutex_unlock(csMapMutex);

		if (type == GEOS_POLYGON)
		{
			return GEOSConvexHull_r(geosCtx, GEOSGeom_createLineString_r(geosCtx, clone));
		}
		else
		{
			return GEOSGeom_createLineString_r(geosCtx, clone);
		}
	}

	return NULL;
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

ERL_NIF_TERM
spatial_tmbr(ErlNifEnv* env, idx_state *pState, int dims, double* mins, double* maxs, double tStart, double tEnd, bool nearest)
{
	ERL_NIF_TERM resultList, sortedList;
	uint64_t nResults;
	IndexItemH* items;
	RTError res;
	double* minsv;
	double* maxsv;

	// force static query objects
	minsv = (double*)calloc(dims, sizeof(double));
	maxsv = (double*)calloc(dims, sizeof(double));

	if (nearest)
	{
		res = Index_TPNearestNeighbors_obj(pState->index, mins, maxs, minsv, maxsv,
								tStart, tEnd, dims, &items, &nResults);
	}
	else
	{
		res = Index_TPIntersects_obj(pState->index, mins, maxs, minsv, maxsv,
								tStart, tEnd, dims, &items, &nResults);
	}

	free(minsv);
	free(maxsv);

	if (res != RT_None)
	{
		char buf[MAXBUFLEN];
		char* pszErrorMsg = Error_GetLastErrorMsg();
		sprintf(buf, "Unable to execute query : %s", pszErrorMsg);
		free(pszErrorMsg);

		return enif_make_tuple2(env, idx_atoms.error,
			enif_make_string(env,
				buf, ERL_NIF_LATIN1));
	}
	else
	{
		resultList = enif_make_list(env, 0);

		for (uint64_t i = 0; i < nResults; i++)
		{
			unsigned char* data = NULL;
			uint64_t len = 0;
			IndexItemH item = items[i];
			if (IndexItem_GetData(item, (uint8_t **)&data, &len) == RT_None)
			{
				ErlNifBinary bin;
				int doc_len;

				// data is a NULL terminated string followed by WKB
				doc_len = strlen((char*)data) + 1;

				if (enif_alloc_binary(doc_len - 1, &bin))
				{
					memcpy(bin.data, data, doc_len - 1);
					ERL_NIF_TERM head = enif_make_binary(env, &bin);
					resultList = enif_make_list_cell(env, head, resultList);
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

					return enif_make_tuple2(env, idx_atoms.error,
						enif_make_string(env, buf, ERL_NIF_LATIN1));
				}
			}
			else
			{
				char buf[MAXBUFLEN];
				char* pszErrorMsg = Error_GetLastErrorMsg();
				sprintf(buf, "Unable to execute query: %s", pszErrorMsg);
				free(pszErrorMsg);

				return enif_make_tuple2(env, idx_atoms.error,
					enif_make_string(env,
					buf, ERL_NIF_LATIN1));
			}

			free(data);
			IndexItem_Destroy(item);
		} // end for loop

		// reverse the results
		enif_make_reverse_list(env, resultList, &sortedList);

		return enif_make_tuple2(env, idx_atoms.ok, sortedList);
	}
}


ERL_NIF_TERM
spatial_mbr(ErlNifEnv* env, idx_state *pState, int dims, double* mins, double* maxs, bool nearest)
{
	ERL_NIF_TERM resultList, sortedList;
	uint64_t nResults;
	IndexItemH* items;
	RTError res;

	if (nearest)
	{
		res = Index_NearestNeighbors_obj(pState->index, mins, maxs,
								dims, &items, &nResults);
	}
	else
	{
		res = Index_Intersects_obj(pState->index, mins, maxs,
								dims, &items, &nResults);
	}

	if (res != RT_None)
	{
		char buf[MAXBUFLEN];
		char* pszErrorMsg = Error_GetLastErrorMsg();
		sprintf(buf, "Unable to execute query : %s", pszErrorMsg);
		free(pszErrorMsg);

		return enif_make_tuple2(env, idx_atoms.error,
			enif_make_string(env,
				buf, ERL_NIF_LATIN1));
	}
	else
	{
		resultList = enif_make_list(env, 0);
		for (uint64_t i = 0; i < nResults; i++)
		{
			unsigned char* data = NULL;
			uint64_t len = 0;
			IndexItemH item = items[i];
			if (IndexItem_GetData(item, (uint8_t **)&data, &len) == RT_None)
			{
				ErlNifBinary bin;
				int doc_len;

				// data is a NULL terminated string followed by WKB
				doc_len = strlen((char*)data) + 1;

				if (enif_alloc_binary(doc_len - 1, &bin))
				{
					memcpy(bin.data, data, doc_len - 1);
					ERL_NIF_TERM head = enif_make_binary(env, &bin);
					resultList = enif_make_list_cell(env, head, resultList);
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

					return enif_make_tuple2(env, idx_atoms.error,
						enif_make_string(env, buf, ERL_NIF_LATIN1));
				}
			}
			else
			{
				char buf[MAXBUFLEN];
				char* pszErrorMsg = Error_GetLastErrorMsg();
				sprintf(buf, "Unable to execute query: %s", pszErrorMsg);
				free(pszErrorMsg);

				return enif_make_tuple2(env, idx_atoms.error,
					enif_make_string(env,
					buf, ERL_NIF_LATIN1));
			}

			free(data);
			IndexItem_Destroy(item);
		} // end for loop

		// reverse the results
		enif_make_reverse_list(env, resultList, &sortedList);

		return enif_make_tuple2(env, idx_atoms.ok, sortedList);
	}
}

ERL_NIF_TERM spatial_function(ErlNifEnv* env, idx_state *pState,
	GEOSGeometry* geom,
	char (*pGEOS_Fun_r)(
			GEOSContextHandle_t,
			const GEOSPreparedGeometry*,
			const GEOSGeometry*
		))
{
	ERL_NIF_TERM resultList;
	const GEOSPreparedGeometry* pg = NULL;
	uint64_t nResults;
	IndexItemH* items;
	uint32_t dims;

	pg = GEOSPrepare_r(pState->geosCtx, geom);

	// calculate mins and maxs from input geometry
	// dims is dependent on the index used
	IndexPropertyH props = Index_GetProperties(pState->index);
	dims = IndexProperty_GetDimension(props);

	double mins[dims];
	double maxs[dims];

	get_min_max(pState->geosCtx, geom, mins, maxs, dims);

	if (Index_Intersects_obj(pState->index, mins, maxs,
							 dims, &items, &nResults) != RT_None)
	{
		GEOSPreparedGeom_destroy_r(pState->geosCtx, pg);

		char buf[MAXBUFLEN];
		char* pszErrorMsg = Error_GetLastErrorMsg();
		sprintf(buf, "Unable to execute query : %s", pszErrorMsg);
		free(pszErrorMsg);

		return enif_make_tuple2(env, idx_atoms.error,
			enif_make_string(env,
				buf, ERL_NIF_LATIN1));
	}

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

					GEOSPreparedGeom_destroy_r(pState->geosCtx, pg);

					return enif_make_tuple2(env, idx_atoms.error,
						enif_make_string(env, buf, ERL_NIF_LATIN1));
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

				GEOSPreparedGeom_destroy_r(pState->geosCtx, pg);

				return enif_make_tuple2(env, idx_atoms.error,
					enif_make_string(env, buf, ERL_NIF_LATIN1));
			}

			free(data);
			IndexItem_Destroy(item);
		}
		else
		{
			char buf[MAXBUFLEN];
			char* pszErrorMsg = Error_GetLastErrorMsg();
			sprintf(buf, "Unable to execute query: %s", pszErrorMsg);
			free(pszErrorMsg);

			return enif_make_tuple2(env, idx_atoms.error,
				enif_make_string(env,
					buf, ERL_NIF_LATIN1));
		}
	} // end for loop

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
