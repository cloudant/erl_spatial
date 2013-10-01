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

% Index Properties
-define(IDX_INDEXTYPE, 0).
-define(IDX_DIMENSION, 1).
-define(IDX_VARIANT, 2).
-define(IDX_STORAGE, 3).
-define(IDX_PAGESIZE, 4).
-define(IDX_INDEXCAPACITY, 5).
-define(IDX_LEAFCAPACITY, 6).
-define(IDX_LEAFPOOLCAPACITY, 7).
-define(IDX_INDEXPOOLCAPACITY, 8).
-define(IDX_REGIONPOOLCAPACITY, 9).
-define(IDX_POINTPOOLCAPACITY, 10).
-define(IDX_BUFFERINGCAPACITY, 11).
-define(IDX_TIGHTMBRS, 12).
-define(IDX_OVERWRITE, 13).
-define(IDX_NEARMINIMUMOVERLAPFACTOR, 14).
-define(IDX_WRITETHROUGH, 15).
-define(IDX_FILLFACTOR, 16).
-define(IDX_SPLITDISTNFACTOR, 17).
-define(IDX_TPRHORIZON, 18).
-define(IDX_REINSERTFACTOR, 19).
-define(IDX_FILENAME, 20).
-define(IDX_FILENAMEEXTDAT, 21).
-define(IDX_FILENAMEEXTIDX, 22).
-define(IDX_INDEXID, 23).
-define(IDX_RESULTLIMIT, 24).

-define(IDX_DEFAULTLIMIT, 200).


% RTIndexType
-define(IDX_RTREE, 0).
-define(IDX_MVRTREE, 1).
-define(IDX_TPRTREE, 2).

% RTIndexVariant
-define(IDX_LINEAR, 0).
-define(IDX_QUADRATIC, 1).
-define(IDX_STAR, 2).

% RTStorageType
-define(IDX_MEMORY, 0).
-define(IDX_DISK, 1).
-define(IDX_CUSTOM, 2).

% CRS
-define(WGS84_LL, "urn:ogc:def:crs:EPSG::4326").

% function
-define(ST_INTERSECTS_MBR, 0).
-define(ST_INTERSECTS, 1).
-define(ST_CONTAINS, 2).
-define(ST_CONTAINS_PROPERLY, 3).
-define(ST_COVERED_BY, 4).
-define(ST_COVERS, 5).
-define(ST_CROSSES, 6).
-define(ST_DISJOINT, 7).
-define(ST_OVERLAPS, 8).
-define(ST_TOUCHES, 9).
-define(ST_WITHIN, 10).
