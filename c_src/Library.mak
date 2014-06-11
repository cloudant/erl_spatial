
C_FLG = -c -DGCC_3 -D__CPP__ -Wall -O3 -I../Include -fPIC

CPP_FLG = -c -DGCC_3 -D__CPP__ -Wall -O3 -I../Include -fPIC

.cpp.o:
	$(CXX) $(CPP_FLG) $<

.c.o:
	$(CC) $(C_FLG) $<

CSMAP_LIB_SRC = \
	csIoUtil.cpp \
	CS_alber.c \
	CS_angle.c \
	CS_ansi.c \
	CS_ats77New.c \
	CS_azmea.c \
	CS_azmed.c \
	CS_badekas.c \
	CS_bonne.c \
	CS_bpcnc.c \
	CS_bursa.c \
	CS_bynFile.c \
	CS_category.c \
	cs_ctio.c \
	CS_csini.c \
	CS_csio.c \
	CS_csprm.c \
	CS_csWktLoc.c \
	CS_datum.c \
	CS_defaults.c \
	CS_defCmp.c \
	CS_defCmpEx.c \
	CS_dtcalc.c \
	CS_dtio.c \
	CS_dtmBridge.c \
	CS_edcnc.c \
	CS_edcyl.c \
	CS_egm96.c \
	CS_ekrt4.c \
	CS_ekrt6.c \
	CS_elCalc.c \
	CS_elio.c \
	CS_erpt.c \
	CS_error.c \
	CS_fips.c \
	CS_frame.c \
	CS_frnch.c \
	CS_gauss.c \
	CS_general.c \
	CS_geocn.c \
	CS_geoct.c \
	CS_geoid96.c \
	CS_geoid99.c \
	CS_GeoidHeight.c \
	CS_gissupprt.c \
	CS_gnomc.c \
	CS_gpio.c \
	CS_gridi.c \
	CS_groups.c \
	CS_guiApi.c \
	CS_gxIndex.c \
	CS_gxio.c \
	CS_gxprm.c \
	CS_hlApi.c \
	CS_hmlsn.c \
	CS_hpApi.c \
	CS_japanNew.c \
	CS_krovk.c \
	CS_lmbrt.c \
	CS_lmtan.c \
	CS_mfc.cpp \
	CS_mgrs.c \
	CS_millr.c \
	CS_modpc.c \
	CS_molod.c \
	CS_molwd.c \
	CS_mrcat.c \
	CS_mstro.c \
	CS_mulrg.c \
	CS_nacyl.c \
	CS_nadcn.c \
	CS_nerth.c \
	CS_ntv1.c \
	CS_ntv2.c \
	CS_nullx.c \
	CS_nzlnd.c \
	CS_oblqm.c \
	CS_optional.c \
	CS_ortho.c \
	CS_osgm91.c \
	CS_ost02.c \
	CS_ost97.c \
	CS_ostn02.c \
	CS_ostn97.c \
	CS_ostro.c \
	CS_parm3.c \
	CS_parm4.c \
	CS_parm6.c \
	CS_parm7.c \
	CS_plycn.c \
	CS_pstro.c \
	CS_rlsUpdt.c \
	CS_robin.c \
	CS_sinus.c \
	CS_sstro.c \
	CS_supprt.c \
	CS_swiss.c \
	CS_sys34.c \
	CS_system.c \
	CS_tacyl.c \
	CS_trmer.c \
	CS_trmrs.c \
	CS_units.c \
	CS_unity.c \
	CS_vdgrn.c \
	CS_VertconUS.c \
	CS_vrtcon.c \
	cs_wellknowntext.cpp \
	CS_wgs72.c \
	CS_winkelTripel.c \
	CS_zones.c \
	csBrowser.cpp \
	CScs2Wkt.cpp \
	cscscomp.c \
	csCsvFileSupport.cpp \
	CSdata.c \
	csDataDir.cpp \
	CSdataDT.c \
	CSdataPJ.c \
	CSdataU.c \
	CSdatumCatalog.c \
	CSdictDiff.c \
	CSdt2Wkt.cpp \
	CSdtcomp.c \
	csDualBrowse.cpp \
	csEdit.cpp \
	CSel2Wkt.cpp \
	CSelcomp.c \
	csEpsgStuff.cpp \
	csEpsgSupport.cpp \
	CSgeodeticSupport.c \
	CSgpcomp.c \
	CSgxcomp.c \
	csKeyNm.cpp \
	CSmrcomp.c \
	csNameMapper.cpp \
	csNameMapperSupport.cpp \
	csTest.cpp \
	CSwinHlp.cpp \
	CSwktFlavors.c \
	dtEdit.cpp \
	dtSelect.cpp \
	elEdit.cpp \
	elSelect.cpp \
	gdcEdit.cpp \
	mgTest.cpp \
	rcWellKnownText.cpp \
	rcWktKonstants.cpp

CSMAP_LIB_OBJ = \
	csIoUtil.o \
	CS_alber.o \
	CS_angle.o \
	CS_ansi.o \
	CS_ats77New.o \
	CS_azmea.o \
	CS_azmed.o \
	CS_badekas.o \
	CS_bonne.o \
	CS_bpcnc.o \
	CS_bursa.o \
	CS_bynFile.o \
	CS_category.o \
	cs_ctio.o \
	CS_csini.o \
	CS_csio.o \
	CS_csprm.o \
	CS_csWktLoc.o \
	CS_datum.o \
	CS_defaults.o \
	CS_defCmp.o \
	CS_defCmpEx.o \
	CS_dtcalc.o \
	CS_dtio.o \
	CS_dtmBridge.o \
	CS_edcnc.o \
	CS_edcyl.o \
	CS_egm96.o \
	CS_ekrt4.o \
	CS_ekrt6.o \
	CS_elCalc.o \
	CS_elio.o \
	CS_erpt.o \
	CS_error.o \
	CS_fips.o \
	CS_frame.o \
	CS_frnch.o \
	CS_gauss.o \
	CS_general.o \
	CS_geocn.o \
	CS_geoct.o \
	CS_geoid96.o \
	CS_geoid99.o \
	CS_GeoidHeight.o \
	CS_gissupprt.o \
	CS_gnomc.o \
	CS_gpio.o \
	CS_gridi.o \
	CS_groups.o \
	CS_guiApi.o \
	CS_gxIndex.o \
	CS_gxio.o \
	CS_gxprm.o \
	CS_hlApi.o \
	CS_hmlsn.o \
	CS_hpApi.o \
	CS_japanNew.o \
	CS_krovk.o \
	CS_lmbrt.o \
	CS_lmtan.o \
	CS_mfc.o \
	CS_mgrs.o \
	CS_millr.o \
	CS_modpc.o \
	CS_molod.o \
	CS_molwd.o \
	CS_mrcat.o \
	CS_mstro.o \
	CS_mulrg.o \
	CS_nacyl.o \
	CS_nadcn.o \
	CS_nerth.o \
	CS_ntv1.o \
	CS_ntv2.o \
	CS_nullx.o \
	CS_nzlnd.o \
	CS_oblqm.o \
	CS_optional.o \
	CS_ortho.o \
	CS_osgm91.o \
	CS_ost02.o \
	CS_ost97.o \
	CS_ostn02.o \
	CS_ostn97.o \
	CS_ostro.o \
	CS_parm3.o \
	CS_parm4.o \
	CS_parm6.o \
	CS_parm7.o \
	CS_plycn.o \
	CS_pstro.o \
	CS_rlsUpdt.o \
	CS_robin.o \
	CS_sinus.o \
	CS_sstro.o \
	CS_supprt.o \
	CS_swiss.o \
	CS_sys34.o \
	CS_system.o \
	CS_tacyl.o \
	CS_trmer.o \
	CS_trmrs.o \
	CS_units.o \
	CS_unity.o \
	CS_vdgrn.o \
	CS_VertconUS.o \
	CS_vrtcon.o \
	cs_wellknowntext.o \
	CS_wgs72.o \
	CS_winkelTripel.o \
	CS_zones.o \
	csBrowser.o \
	CScs2Wkt.o \
	cscscomp.o \
	csCsvFileSupport.o \
	CSdata.o \
	csDataDir.o \
	CSdataDT.o \
	CSdataPJ.o \
	CSdataU.o \
	CSdatumCatalog.o \
	CSdictDiff.o \
	CSdt2Wkt.o \
	CSdtcomp.o \
	csDualBrowse.o \
	csEdit.o \
	CSel2Wkt.o \
	CSelcomp.o \
	csEpsgStuff.o \
	csEpsgSupport.o \
	CSgeodeticSupport.o \
	CSgpcomp.o \
	CSgxcomp.o \
	csKeyNm.o \
	CSmrcomp.o \
	csNameMapper.o \
	csNameMapperSupport.o \
	csTest.o \
	CSwinHlp.o \
	CSwktFlavors.o \
	dtEdit.o \
	dtSelect.o \
	elEdit.o \
	elSelect.o \
	gdcEdit.o \
	mgTest.o \
	rcWellKnownText.o \
	rcWktKonstants.o

CsMap.a : $(CSMAP_LIB_OBJ)
	ar rv CsMap.a $?

clean:
	rm -f $(CSMAP_LIB_OBJ)
	rm -f CsMap.a

rebuild: clean CsMap.a
