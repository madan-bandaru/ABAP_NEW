class ZCL_FI_CFIN_DOCUMENT definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_FIN_CFIN_DOCUMENT .
  interfaces IF_BADI_INTERFACE .

  class-methods RTR_LOAD_AUAK_WBS
    importing
      !IS_AUAK type AUAK .
protected section.
private section.

  types:
    BEGIN OF ts_auak_wbs,
      awref TYPE awref,
      posid TYPE ps_posid,
    END OF ts_auak_wbs .
  types:
    BEGIN OF ts_anla,
      bukrs TYPE bukrs,
      anln1 TYPE anln1,
      anln2 TYPE anln2,
      anlkl TYPE anlkl,
    END OF ts_anla .
  types:
    tt_auak_wbs TYPE SORTED TABLE OF ts_auak_wbs WITH UNIQUE KEY primary_key COMPONENTS awref .
  types:
    TT_ANLA TYPE TABLE OF ts_anla .

  constants:
    BEGIN OF c_otype,
      pr   TYPE char2 VALUE 'PR' ##NO_TEXT,
      nv   TYPE char2 VALUE 'NV' ##NO_TEXT,
      np   TYPE char2 VALUE 'NP' ##NO_TEXT,
      none TYPE char2 VALUE '' ##NO_TEXT,
    END OF c_otype .
  class-data ST_AUAK_WBS type TT_AUAK_WBS .
  constants:
    BEGIN OF c_package_id,
      fido TYPE char1 VALUE 'D' ##NO_TEXT,
    END OF c_package_id .
  constants:
    BEGIN OF c_awtyp,
      auak TYPE awtyp VALUE 'AUAK' ##NO_TEXT,
    END OF c_awtyp .
  constants:
    BEGIN OF c_objnr_type,
      pr TYPE char2 VALUE 'PR' ##NO_TEXT,
    END OF c_objnr_type .
  constants C_SGTXT_PREFIX_WBS type CHAR3 value 'WBS' ##NO_TEXT.
  data:
    BEGIN OF ms_afvc,
      aufpl TYPE co_aufpl,
      aplzl TYPE co_aplzl,
      posid TYPE ps_posid,
      objnr TYPE j_objnr,
    END OF ms_afvc .
  data:
    BEGIN OF ms_aufk,
      aufnr TYPE aufnr,
      posid TYPE ps_posid,
      objnr TYPE j_objnr,
    END OF ms_aufk .

  methods NETWORK_TO_WBS
    importing
      !IV_AUFPL type CO_AUFPL
      !IV_APLZL type CO_APLZL
      !IV_NPLNR type NPLNR
      !IV_PS_PSP_PNR type PS_PSP_PNR optional
    exporting
      !EV_POSID type PS_POSID
      !EV_OBJNR type J_OBJNR
    changing
      !CV_UPDATE type BOOLE_D .
  methods READ_AFVC
    importing
      !IV_AUFPL type CO_AUFPL
      !IV_APLZL type CO_APLZL
    exporting
      !EV_POSID type PS_POSID
      !EV_OBJNR type J_OBJNR .
  methods READ_AUFK
    importing
      !IV_NPLNR type NPLNR
    exporting
      !EV_POSID type PS_POSID
      !EV_OBJNR type J_OBJNR .
  methods GET_AUAK_WBS
    importing
      !IV_AWREF type AWREF
      !IV_SGTXT type SGTXT
    changing
      !CV_UPDATE type BOOLE_D
      !CV_POSID type PS_POSID .
  methods IL_LOAD_AUAK_WBS
    importing
      !IV_PACKAGE_ID type CHAR1
      value(IT_ACCHD) type FIN_CFIN_T_CFIN_ACCHD .
  methods IL_LOAD_ANLKL
    importing
      value(IT_ACCIT) type FIN_CFIN_T_CFIN_ACCIT optional
    exporting
      value(IT_ANLA) type TT_ANLA .
  methods RTR_LOAD_ANLKL
    importing
      value(IT_ACCIT) type FIN_CFIN_T_ACCIT optional
    exporting
      value(IT_ANLA) type TT_ANLA .
  methods GET_ASSET_CLASS
    importing
      value(IV_BUKRS) type BUKRS optional
      value(IV_ANLN1) type ANLN1 optional
      value(IV_ANLN2) type ANLN2 optional
      value(IT_ANLA) type TT_ANLA optional
    exporting
      value(EV_ANLKL) type ANLKL
    changing
      value(CV_UPDATE) type FLAG optional .
ENDCLASS.



CLASS ZCL_FI_CFIN_DOCUMENT IMPLEMENTATION.


  method GET_ASSET_CLASS.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... GET_ASSET_CLASS
* TITLE......... Replicate asset class in CFIN
* AUTHOR........ Shashank P(INSPW5)
* DATE WRITTEN.. 12-Jul-2021
* R/3 RELEASE... NA
* REV-TRAC...... NA
* TRANSPORT.....
* COPIED FROM... NA
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
*
*----------------------------------------------------------------------*
* PROGRAM TYPE.. CLASS METHOD
* PACKAGE.......
* LOGICAL DB.... N/A
************************************************************************

************************************************************************
* CHANGE HISTORY                                                       *
************************************************************************
* DATE CHANGE...
* AUTHOR........
* CHANGE DESCR..
* R/3 RELEASE...
* REV TRAC NO...
************************************************************************
    TRY.
        ev_anlkl = it_anla[ bukrs = iv_bukrs anln1 = iv_anln1 anln2 = iv_anln2 ]-anlkl.
        cv_update = abap_true.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

  endmethod.


  METHOD GET_AUAK_WBS.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... GET_AUAK_WBS
* TITLE......... Add settled WBS details
* AUTHOR........ Pulock Chamuah(INPCK1)
* DATE WRITTEN.. 01-May-2021
* R/3 RELEASE... NA
* REV-TRAC...... NA
* TRANSPORT..... S94K934874
* COPIED FROM... NA
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* This is to add settled WBS details
*----------------------------------------------------------------------*
* PROGRAM TYPE.. CLASS METHOD
* PACKAGE....... /DS1/FSS_01
* LOGICAL DB.... N/A
************************************************************************

************************************************************************
* CHANGE HISTORY                                                       *
************************************************************************
* DATE CHANGE...
* AUTHOR........
* CHANGE DESCR..
* R/3 RELEASE...
* REV TRAC NO...
************************************************************************

    "check if details exists
    CHECK me->st_auak_wbs[] IS NOT INITIAL.

    "check if WBS is the settled cost object
    CHECK iv_sgtxt+0(3) EQ me->c_sgtxt_prefix_wbs.

    "read POSID details
    TRY .
        cv_posid = me->st_auak_wbs[ KEY primary_key COMPONENTS awref = iv_awref ]-posid.
        cv_update = abap_true.
      CATCH cx_sy_itab_line_not_found.
        "no action
    ENDTRY.

  ENDMETHOD.


  METHOD IF_BADI_FIN_CFIN_DOCUMENT~CO_DOC_ENHANCE.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... IF_BADI_FIN_CFIN_DOCUMENT~CO_DOC_ENHANCE
* TITLE......... Enhance CO Doc Real Time Replication and Initial Load
* AUTHOR........ Pulock Chamuah (INPCK1)
* DATE WRITTEN.. 28-March-2021
* R/3 RELEASE... CFIN-SBX
* TRANSPORTNR... S94K934874
* COPIED FROM... NA
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* E0009 - Network/Activity is not supported by CFIN.
* Hence, convert to WBS for CO Doc Load.
*----------------------------------------------------------------------*
* PROGRAM TYPE.. Class Method
* DEV. CLASS.... /DS1/FSS_01
* LOGICAL DB.... NA
************************************************************************

************************************************************************
* CHANGE HISTORY
************************************************************************
* DATE CHANGE... <Date of change written>
* AUTHOR........ <Author name>
* CHANGE DESCR.. <Description of change>
* R/3 RELEASE... <Release>
* TRANSPORTNR... <Transport number>
************************************************************************

    "local data declaration
    DATA: ls_co_add LIKE LINE OF et_cfin_co_add,
          lv_update TYPE boole_d.

    "check and update WBS details
    LOOP AT it_coep ASSIGNING FIELD-SYMBOL(<ls_coep>).
      CLEAR ls_co_add.
      "E0009 Network/Activity to WBS
      DATA(lv_otype) = <ls_coep>-objnr+0(2).
      me->network_to_wbs(
        EXPORTING
          iv_aufpl      = COND co_aufpl( WHEN lv_otype EQ me->c_otype-nv THEN <ls_coep>-objnr+2(10) ELSE me->c_otype-none )
          iv_aplzl      = COND co_aplzl( WHEN lv_otype EQ me->c_otype-nv THEN <ls_coep>-objnr+12(8) ELSE me->c_otype-none )
          iv_nplnr      = COND nplnr( WHEN lv_otype EQ me->c_otype-np THEN <ls_coep>-objnr+2(12) ELSE me->c_otype-none )
          iv_ps_psp_pnr = COND ps_psp_pnr( WHEN lv_otype EQ me->c_otype-pr THEN <ls_coep>-objnr+2(8) ELSE me->c_otype-none )
        IMPORTING
          ev_posid      = ls_co_add-zz_ps_posid
          ev_objnr      = ls_co_add-zz_ps_objnr
        CHANGING
          cv_update     = lv_update ).

      "E0009 Network/Activity to WBS - Partner Object
      lv_otype = <ls_coep>-parob+0(2).
      me->network_to_wbs(
        EXPORTING
          iv_aufpl      = COND co_aufpl( WHEN lv_otype EQ me->c_otype-nv THEN <ls_coep>-parob+2(10) ELSE me->c_otype-none )
          iv_aplzl      = COND co_aplzl( WHEN lv_otype EQ me->c_otype-nv THEN <ls_coep>-parob+12(8) ELSE me->c_otype-none )
          iv_nplnr      = COND nplnr( WHEN lv_otype EQ me->c_otype-np THEN <ls_coep>-parob+2(12) ELSE me->c_otype-none )
        IMPORTING
          ev_objnr      = ls_co_add-zz_parob
        CHANGING
          cv_update     = lv_update ).

      "E0009 Network/Activity to WBS - Partner Object 1
      lv_otype = <ls_coep>-parob1+0(2).
      me->network_to_wbs(
        EXPORTING
          iv_aufpl      = COND co_aufpl( WHEN lv_otype EQ me->c_otype-nv THEN <ls_coep>-parob1+2(10) ELSE me->c_otype-none )
          iv_aplzl      = COND co_aplzl( WHEN lv_otype EQ me->c_otype-nv THEN <ls_coep>-parob1+12(8) ELSE me->c_otype-none )
          iv_nplnr      = COND nplnr( WHEN lv_otype EQ me->c_otype-np THEN <ls_coep>-parob1+2(12) ELSE me->c_otype-none )
        IMPORTING
          ev_objnr      = ls_co_add-zz_parob1
        CHANGING
          cv_update     = lv_update ).

      "update customer fields table
      CHECK lv_update EQ abap_true.

      "get base data of extension structure
      ls_co_add-kokrs = <ls_coep>-kokrs.
      ls_co_add-belnr = <ls_coep>-belnr.
      ls_co_add-buzei = <ls_coep>-buzei.
      ls_co_add-co_cat = |X|.

      "collect
      INSERT ls_co_add INTO TABLE et_cfin_co_add.
      CLEAR lv_update.
    ENDLOOP. "LOOP AT it_coep ASSIGNING FIELD-SYMBOL(<ls_coep>).

  ENDMETHOD.


  METHOD IF_BADI_FIN_CFIN_DOCUMENT~FI_DOC_ENHANCE_INITIAL_LOAD.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... IF_BADI_FIN_CFIN_DOCUMENT~FI_DOC_ENHANCE_INITIAL_LOAD
* TITLE......... Enhance FI Doc Initial Load
* AUTHOR........ Pulock Chamuah (INPCK1)
* DATE WRITTEN.. 28-March-2021
* R/3 RELEASE... CFIN-SBX
* TRANSPORTNR... S94K934874
* COPIED FROM... NA
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* This is to update CFIN document during IL
*----------------------------------------------------------------------*
* PROGRAM TYPE.. Class Method
* DEV. CLASS.... /DS1/FSS_01
* LOGICAL DB.... NA
************************************************************************

************************************************************************
* CHANGE HISTORY
************************************************************************
* DATE CHANGE... <Date of change written>
* AUTHOR........ <Author name>
* CHANGE DESCR.. <Description of change>
* R/3 RELEASE... <Release>
* TRANSPORTNR... <Transport number>
************************************************************************

    "local data declaration
    DATA: lv_update       TYPE boole_d.

    "E0052: Load settled WBS
    me->il_load_auak_wbs(
      EXPORTING
        iv_package_id = iv_package_key+0(1)
        it_acchd      = it_acchd ).

    "E0070: Replicate Asset Class
    me->il_load_anlkl(
      EXPORTING
        it_accit = it_accit
      IMPORTING
        it_anla = DATA(it_anla) ).

    "check and update WBS details
    LOOP AT it_accit_app ASSIGNING FIELD-SYMBOL(<ls_accit_app>).
      "get item reference
      TRY .
          ASSIGN it_accit[ acchd_guid = <ls_accit_app>-acchd_guid itemno = <ls_accit_app>-itemno ]
            TO FIELD-SYMBOL(<ls_accit>).
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      "get base data of extension structure
      DATA(ls_accit_cus_init) = CORRESPONDING fin_cfin_s_accit_app_cus_init( <ls_accit_app> ).

      " E0009 - Network/Activity is not supported by CFIN.
      " Hence, convert to WBS for FI DOc Initial Load.
      me->network_to_wbs(
        EXPORTING
          iv_aufpl      = <ls_accit>-aufpl
          iv_aplzl      = <ls_accit>-aplzl
          iv_nplnr      = <ls_accit>-nplnr
          iv_ps_psp_pnr = <ls_accit>-ps_psp_pnr
        IMPORTING
          ev_posid      = ls_accit_cus_init-zz_ps_posid
          ev_objnr      = ls_accit_cus_init-zz_ps_objnr
        CHANGING
          cv_update     = lv_update ).

      "E0052: add settled WBS
      me->get_auak_wbs(
        EXPORTING
          iv_awref  = ls_accit_cus_init-awref
          iv_sgtxt  = <ls_accit>-sgtxt
        CHANGING
          cv_posid  = ls_accit_cus_init-zz_ps_posid_auak
          cv_update = lv_update ).

      "E0070: Replicate Asset Class
      me->get_asset_class(
        EXPORTING
          iv_bukrs = <ls_accit>-bukrs
          iv_anln1 = <ls_accit>-anln1
          iv_anln2 = <ls_accit>-anln2
          it_anla  = it_anla
        IMPORTING
          ev_anlkl = ls_accit_cus_init-zz_anlkl_anla
        CHANGING
          cv_update = lv_update ).

      "update customer fields table
      CHECK lv_update EQ abap_true.
      INSERT ls_accit_cus_init INTO TABLE et_accit_cus_init.
      CLEAR lv_update.
    ENDLOOP.

  ENDMETHOD.


  METHOD IF_BADI_FIN_CFIN_DOCUMENT~FI_DOC_ENHANCE_ONGOING.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... IF_BADI_FIN_CFIN_DOCUMENT~FI_DOC_ENHANCE_ONGOING
* TITLE......... Enhance FI Doc Real Time Replication
* AUTHOR........ Pulock Chamuah (INPCK1)
* DATE WRITTEN.. 28-March-2021
* R/3 RELEASE... CFIN-SBX
* TRANSPORTNR... S94K934874
* COPIED FROM... NA
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* This is to update CFIN document during RtR
*----------------------------------------------------------------------*
* PROGRAM TYPE.. Class Method
* DEV. CLASS.... /DS1/FSS_01
* LOGICAL DB.... NA
************************************************************************

************************************************************************
* CHANGE HISTORY
************************************************************************
* DATE CHANGE... <Date of change written>
* AUTHOR........ <Author name>
* CHANGE DESCR.. <Description of change>
* R/3 RELEASE... <Release>
* TRANSPORTNR... <Transport number>
************************************************************************

    "local data declaration
    DATA: lv_update       TYPE boole_d.

    "E0070: Replicate Asset Class
    me->rtr_load_anlkl(
      EXPORTING
        it_accit = it_accit
      IMPORTING
        it_anla = DATA(it_anla) ).

    "check and update WBS details
    LOOP AT it_accit_app ASSIGNING FIELD-SYMBOL(<ls_accit_app>).
      "get item reference
      TRY .
          ASSIGN it_accit[ awtyp = <ls_accit_app>-awtyp awref = <ls_accit_app>-awref
            aworg = <ls_accit_app>-aworg posnr = <ls_accit_app>-posnr ]
            TO FIELD-SYMBOL(<ls_accit>).
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      "get base data of extension structure
      DATA(ls_accit_cus) = CORRESPONDING fin_cfin_s_accit_app_cus( <ls_accit_app> ).

      "E0009 - Network/Activity is not supported by CFIN.
      "Hence, convert to WBS for FI Doc Load.
      me->network_to_wbs(
        EXPORTING
          iv_aufpl      = <ls_accit>-aufpl
          iv_aplzl      = <ls_accit>-aplzl
          iv_nplnr      = <ls_accit>-nplnr
          iv_ps_psp_pnr = <ls_accit>-ps_psp_pnr
        IMPORTING
          ev_posid      = ls_accit_cus-zz_ps_posid
          ev_objnr      = ls_accit_cus-zz_ps_objnr
        CHANGING
          cv_update     = lv_update ).

      "E0052: add settled WBS
      me->get_auak_wbs(
        EXPORTING
          iv_awref  = ls_accit_cus-awref
          iv_sgtxt  = <ls_accit>-sgtxt
        CHANGING
          cv_posid  = ls_accit_cus-zz_ps_posid_auak
          cv_update = lv_update ).

      "E0070: Replicate Asset Class
      me->get_asset_class(
        EXPORTING
          iv_bukrs = <ls_accit>-bukrs
          iv_anln1 = <ls_accit>-anln1
          iv_anln2 = <ls_accit>-anln2
          it_anla  = it_anla
        IMPORTING
          ev_anlkl = ls_accit_cus-zz_anlkl_anla
        CHANGING
          cv_update = lv_update ).

      "update customer fields table
      CHECK lv_update EQ abap_true.
      INSERT ls_accit_cus INTO TABLE et_accit_cus.
      CLEAR lv_update.
    ENDLOOP.

  ENDMETHOD.


  method IF_BADI_FIN_CFIN_DOCUMENT~PCA_ENHANCE.
  endmethod.


  method IL_LOAD_ANLKL.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... IL_LOAD_ANLKL
* TITLE......... Replicate asset class in CFIN
* AUTHOR........ Shashank P(INSPW5)
* DATE WRITTEN.. 12-Jul-2021
* R/3 RELEASE... NA
* REV-TRAC...... NA
* TRANSPORT.....
* COPIED FROM... NA
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
*
*----------------------------------------------------------------------*
* PROGRAM TYPE.. CLASS METHOD
* PACKAGE.......
* LOGICAL DB.... N/A
************************************************************************

************************************************************************
* CHANGE HISTORY                                                       *
************************************************************************
* DATE CHANGE...
* AUTHOR........
* CHANGE DESCR..
* R/3 RELEASE...
* REV TRAC NO...
************************************************************************
    CHECK it_accit IS NOT INITIAL.

      SELECT bukrs,
             anln1,
             anln2,
             anlkl FROM anla INTO TABLE @it_anla
             FOR ALL ENTRIES IN @it_accit WHERE bukrs EQ @it_accit-bukrs
             AND anln1 EQ @it_accit-anln1 AND anln2 EQ @it_accit-anln2.

  endmethod.


  METHOD IL_LOAD_AUAK_WBS.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... IL_LOAD_AUAK_WBS
* TITLE......... Load Settled WBS details during IL
* AUTHOR........ Pulock Chamuah(INPCK1)
* DATE WRITTEN.. 01-May-2021
* R/3 RELEASE... NA
* REV-TRAC...... NA
* TRANSPORT..... S94K934874
* COPIED FROM... NA
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* This is to load settled WBS details during IL
*----------------------------------------------------------------------*
* PROGRAM TYPE.. CLASS METHOD
* PACKAGE....... /DS1/FSS_01
* LOGICAL DB.... N/A
************************************************************************

************************************************************************
* CHANGE HISTORY                                                       *
************************************************************************
* DATE CHANGE...
* AUTHOR........
* CHANGE DESCR..
* R/3 RELEASE...
* REV TRAC NO...
************************************************************************

    "clear data container
    CLEAR me->st_auak_wbs[].

    "applicable for FIDO only
    CHECK iv_package_id EQ me->c_package_id-fido.

    "applicable for AUAK postings only
    DELETE it_acchd WHERE awtyp NE me->c_awtyp-auak.
    CHECK it_acchd[] IS NOT INITIAL.

    "get unvonverted WBS
    SELECT a~belnr AS awref b~posid INTO TABLE me->st_auak_wbs
      FROM auak AS a INNER JOIN prps AS b ON a~objnr EQ b~objnr
      FOR ALL ENTRIES IN it_acchd
      WHERE a~belnr EQ it_acchd-awref
        AND a~objnr LIKE 'PR%' ##NO_TEXT.

  ENDMETHOD.


  METHOD NETWORK_TO_WBS.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... /DS1/CL_FI_CFIN_DOCUMENT->FI_DOC_ILD_NETWORK_TO_WBS
* TITLE......... Convert Network to WBS for FI Doc Load
* AUTHOR........ Pulock Chamuah (INPCK1)
* DATE WRITTEN.. 28-March-2021
* R/3 RELEASE... CFIN-SBX
* TRANSPORTNR... S94K934874
* COPIED FROM... NA
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* E0009 - Network/Activity is not supported by CFIN.
* Hence, convert to WBS for FI Doc Load.
*----------------------------------------------------------------------*
* PROGRAM TYPE.. Class Method
* DEV. CLASS.... /DS1/FSS_01
* LOGICAL DB.... NA
************************************************************************

************************************************************************
* CHANGE HISTORY
************************************************************************
* DATE CHANGE... <Date of change written>
* AUTHOR........ <Author name>
* CHANGE DESCR.. <Description of change>
* R/3 RELEASE... <Release>
* TRANSPORTNR... <Transport number>
************************************************************************

    "WBS: if available continue with next item
    CHECK iv_ps_psp_pnr IS INITIAL.

    "Activity: if available get WBS
    IF iv_aufpl IS NOT INITIAL AND iv_aplzl IS NOT INITIAL.
      me->read_afvc(
        EXPORTING
          iv_aufpl = iv_aufpl
          iv_aplzl = iv_aplzl
        IMPORTING
          ev_posid = ev_posid
          ev_objnr = ev_objnr ).
    ENDIF.

    "Network: if available get WBS
    IF ev_posid IS INITIAL AND iv_nplnr IS NOT INITIAL.
      me->read_aufk(
        EXPORTING
          iv_nplnr = iv_nplnr
        IMPORTING
          ev_posid = ev_posid
          ev_objnr = ev_objnr ).
    ENDIF.

    "set update flag
    CHECK ev_posid IS NOT INITIAL.
    cv_update = abap_true.

  ENDMETHOD.


  METHOD READ_AFVC.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... /DS1/CL_FI_CFIN_DOCUMENT->READ_AFVC
* TITLE......... Activity to WBS
* AUTHOR........ Pulock Chamuah (INPCK1)
* DATE WRITTEN.. 28-March-2021
* R/3 RELEASE... CFIN-SBX
* TRANSPORTNR... S94K934874
* COPIED FROM... NA
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* E0009 - Network/Activity is not supported by CFIN.
* Hence, convert to WBS for FI Doc Load.
*----------------------------------------------------------------------*
* PROGRAM TYPE.. Class Method
* DEV. CLASS.... /DS1/FSS_01
* LOGICAL DB.... NA
************************************************************************

************************************************************************
* CHANGE HISTORY
************************************************************************
* DATE CHANGE... <Date of change written>
* AUTHOR........ <Author name>
* CHANGE DESCR.. <Description of change>
* R/3 RELEASE... <Release>
* TRANSPORTNR... <Transport number>
************************************************************************

    "read Activity
    IF iv_aufpl NE me->ms_afvc-aufpl OR iv_aplzl NE me->ms_afvc-aplzl.
      CLEAR me->ms_afvc.

      "read WBS associsted with activity
      SELECT SINGLE projn INTO @DATA(lv_projn) FROM afvc
        WHERE aufpl EQ @iv_aufpl
          AND aplzl EQ @iv_aplzl.

      "get WBS internal and Object ID
      CHECK lv_projn IS NOT INITIAL.
      SELECT SINGLE posid, objnr INTO (@me->ms_afvc-posid, @me->ms_afvc-objnr) FROM prps
        WHERE pspnr EQ @lv_projn.
      me->ms_afvc-aufpl = iv_aufpl.
      me->ms_afvc-aplzl = iv_aplzl.
    ENDIF.

    "return data
    ev_posid = me->ms_afvc-posid.
    ev_objnr = me->ms_afvc-objnr.

  ENDMETHOD.


  METHOD READ_AUFK.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... /DS1/CL_FI_CFIN_DOCUMENT->READ_AUFK
* TITLE......... Network to WBS
* AUTHOR........ Pulock Chamuah (INPCK1)
* DATE WRITTEN.. 28-March-2021
* R/3 RELEASE... CFIN-SBX
* TRANSPORTNR... S94K934874
* COPIED FROM... NA
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* E0009 - Network/Activity is not supported by CFIN.
* Hence, convert to WBS for FI Doc Load.
*----------------------------------------------------------------------*
* PROGRAM TYPE.. Class Method
* DEV. CLASS.... /DS1/FSS_01
* LOGICAL DB.... NA
************************************************************************

************************************************************************
* CHANGE HISTORY
************************************************************************
* DATE CHANGE... <Date of change written>
* AUTHOR........ <Author name>
* CHANGE DESCR.. <Description of change>
* R/3 RELEASE... <Release>
* TRANSPORTNR... <Transport number>
************************************************************************

    "read Network
    IF me->ms_aufk-aufnr NE iv_nplnr.
      CLEAR me->ms_aufk.

      "read WBS associsted with activity
      SELECT SINGLE pspel INTO @DATA(lv_pspel) FROM aufk
        WHERE aufnr EQ @iv_nplnr.

      "get WBS external and Object ID
      CHECK lv_pspel IS NOT INITIAL.
      SELECT SINGLE posid, objnr INTO (@me->ms_aufk-posid, @me->ms_aufk-objnr) FROM prps
        WHERE pspnr EQ @lv_pspel.
      me->ms_aufk-aufnr = iv_nplnr.
    ENDIF.

    "return data
    ev_posid = me->ms_aufk-posid.
    ev_objnr = me->ms_aufk-objnr.

  ENDMETHOD.


  method RTR_LOAD_ANLKL.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... IL_LOAD_ANLKL
* TITLE......... Replicate asset class in CFIN
* AUTHOR........ Shashank P(INSPW5)
* DATE WRITTEN.. 12-Jul-2021
* R/3 RELEASE... NA
* REV-TRAC...... NA
* TRANSPORT.....
* COPIED FROM... NA
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
*
*----------------------------------------------------------------------*
* PROGRAM TYPE.. CLASS METHOD
* PACKAGE.......
* LOGICAL DB.... N/A
************************************************************************

************************************************************************
* CHANGE HISTORY                                                       *
************************************************************************
* DATE CHANGE...
* AUTHOR........
* CHANGE DESCR..
* R/3 RELEASE...
* REV TRAC NO...
************************************************************************
    CHECK it_accit IS NOT INITIAL.

      SELECT bukrs,
             anln1,
             anln2,
             anlkl FROM anla INTO TABLE @it_anla
             FOR ALL ENTRIES IN @it_accit WHERE bukrs EQ @it_accit-bukrs
             AND anln1 EQ @it_accit-anln1 AND anln2 EQ @it_accit-anln2.
  endmethod.


  METHOD RTR_LOAD_AUAK_WBS.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... RTR_LOAD_AUAK_WBS
* TITLE......... Load Settled WBS details during RtR
* AUTHOR........ Pulock Chamuah(INPCK1)
* DATE WRITTEN.. 01-May-2021
* R/3 RELEASE... NA
* REV-TRAC...... NA
* TRANSPORT..... S94K934874
* COPIED FROM... NA
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* This is to load settled WBS details during RtR
*----------------------------------------------------------------------*
* PROGRAM TYPE.. CLASS METHOD
* PACKAGE....... /DS1/FSS_01
* LOGICAL DB.... N/A
************************************************************************

************************************************************************
* CHANGE HISTORY                                                       *
************************************************************************
* DATE CHANGE...
* AUTHOR........
* CHANGE DESCR..
* R/3 RELEASE...
* REV TRAC NO...
************************************************************************

    "clear parameter
    CLEAR st_auak_wbs[].

    "get unvonverted WBS
    CHECK is_auak-objnr+0(2) EQ c_objnr_type-pr.
    INSERT VALUE #( awref = is_auak-belnr ) INTO TABLE st_auak_wbs ASSIGNING FIELD-SYMBOL(<ls_auak_wbs>).
    CHECK sy-subrc IS INITIAL.
    SELECT SINGLE posid INTO <ls_auak_wbs>-posid FROM prps WHERE objnr EQ is_auak-objnr.

  ENDMETHOD.
ENDCLASS.
