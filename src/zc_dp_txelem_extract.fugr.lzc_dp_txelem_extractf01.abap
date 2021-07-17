*----------------------------------------------------------------------*
***INCLUDE /DS1/LC_SETI_EXTRACTF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DYNAMIC_WHERE_CLASS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
************************************************************************
* CHANGE HISTORY                                                       *
************************************************************************
* DATE CHANGE.....: 16.11.2016                                         *
* AUTHOR..........: Santhosha Rangavala(INSRPK)                        *
* CHANGE DESCR....: Incorporating the BKPF select in form              *
*                   FETCH_REQUIRED_DATA into the OPEN-CURSOR to avoid  *
*                   TIME_OUT Dump                                      *
* MODIFICATION ID : MOD-001                                            *
* SERVICE CENT REF: GPM30069873-001                                    *
* RT/TRANSPORT... :90264/ D94K9A78CV                                   *
************************************************************************
FORM dynamic_where_class.

  REFRESH: t_components[].
  CLEAR: w_component, g_wa, g_tab.

* determine components of structure -> t_COMPONENTS
  MOVE 'SIGN' TO w_component-name.
*  w_component-type ?= cl_abap_elemdescr=>get_c( p_length = 1 ).
  MOVE cl_abap_elemdescr=>get_c( p_length = 1 ) ?TO w_component-type.
  INSERT w_component INTO TABLE t_components.

  MOVE 'OPTION' TO w_component-name.
  MOVE cl_abap_elemdescr=>get_c( p_length = 2 ) ?TO w_component-type.
  INSERT w_component INTO TABLE t_components.

  MOVE 'LOW' TO w_component-name.
  MOVE cl_abap_elemdescr=>describe_by_name( w_dfies-rollname ) ?TO w_component-type.
  INSERT w_component INTO TABLE t_components.

  MOVE 'HIGH' TO w_component-name.
  MOVE cl_abap_elemdescr=>describe_by_name( w_dfies-rollname ) ?TO w_component-type.
  INSERT w_component INTO TABLE t_components.

* get structure descriptor -> g_STRUCTDESCR
  MOVE cl_abap_structdescr=>create( t_components ) ?TO g_structdescr.

  MOVE g_structdescr TO g_datadescr.
  MOVE cl_abap_tabledescr=>create( g_datadescr ) ?TO g_tabledescr.

  g_count = g_count + 1.

  CASE g_count.
    WHEN 1.
* create work area of structure g_STRUCTDESCR -> g_WA
      CREATE DATA g_wa TYPE HANDLE g_structdescr.
      ASSIGN g_wa->* TO <fs_wa1>.
      MOVE-CORRESPONDING w_select TO <fs_wa1>.

* Create dynmaic internal table
      CREATE DATA g_tab TYPE HANDLE g_tabledescr.
      ASSIGN g_tab->* TO <fs_tab1>.
      APPEND <fs_wa1> TO <fs_tab1>.

      CONCATENATE 'and'(001) w_dfies-fieldname 'in <fs_tab1>'(002) INTO w_where SEPARATED BY space.
      APPEND w_where TO t_where.

    WHEN 2.
* create work area of structure g_STRUCTDESCR -> g_WA
      CREATE DATA g_wa TYPE HANDLE g_structdescr.
      ASSIGN g_wa->* TO <fs_wa2>.
      MOVE-CORRESPONDING w_select TO <fs_wa2>.

* Create dynmaic internal table
      CREATE DATA g_tab TYPE HANDLE g_tabledescr.
      ASSIGN g_tab->* TO <fs_tab2>.
      APPEND <fs_wa2> TO <fs_tab2>.

      CONCATENATE 'and' w_dfies-fieldname 'in <fs_tab2>'(003) INTO w_where SEPARATED BY space.
      APPEND w_where TO t_where.

    WHEN 3.
* create work area of structure g_STRUCTDESCR -> g_WA
      CREATE DATA g_wa TYPE HANDLE g_structdescr.
      ASSIGN g_wa->* TO <fs_wa3>.
      MOVE-CORRESPONDING w_select TO <fs_wa3>.

* Create dynmaic internal table
      CREATE DATA g_tab TYPE HANDLE g_tabledescr.
      ASSIGN g_tab->* TO <fs_tab3>.
      APPEND <fs_wa3> TO <fs_tab3>.

      CONCATENATE 'and' w_dfies-fieldname 'in <fs_tab3>'(004) INTO w_where SEPARATED BY space.
      APPEND w_where TO t_where.

    WHEN 4.
* create work area of structure g_STRUCTDESCR -> g_WA
      CREATE DATA g_wa TYPE HANDLE g_structdescr.
      ASSIGN g_wa->* TO <fs_wa4>.
      MOVE-CORRESPONDING w_select TO <fs_wa4>.

* Create dynmaic internal table
      CREATE DATA g_tab TYPE HANDLE g_tabledescr.
      ASSIGN g_tab->* TO <fs_tab4>.
      APPEND <fs_wa4> TO <fs_tab4>.

      CONCATENATE 'and' w_dfies-fieldname 'in <fs_tab4>'(005) INTO w_where SEPARATED BY space.
      APPEND w_where TO t_where.

    WHEN 5.
* create work area of structure g_STRUCTDESCR -> g_WA
      CREATE DATA g_wa TYPE HANDLE g_structdescr.
      ASSIGN g_wa->* TO <fs_wa5>.
      MOVE-CORRESPONDING w_select TO <fs_wa5>.

* Create dynmaic internal table
      CREATE DATA g_tab TYPE HANDLE g_tabledescr.
      ASSIGN g_tab->* TO <fs_tab5>.
      APPEND <fs_wa5> TO <fs_tab5>.

      CONCATENATE 'and' w_dfies-fieldname 'in <fs_tab5>'(006) INTO w_where SEPARATED BY space.
      APPEND w_where TO t_where.

    WHEN 6.
* create work area of structure g_STRUCTDESCR -> g_WA
      CREATE DATA g_wa TYPE HANDLE g_structdescr.
      ASSIGN g_wa->* TO <fs_wa6>.
      MOVE-CORRESPONDING w_select TO <fs_wa6>.

* Create dynmaic internal table
      CREATE DATA g_tab TYPE HANDLE g_tabledescr.
      ASSIGN g_tab->* TO <fs_tab6>.
      APPEND <fs_wa6> TO <fs_tab6>.

      CONCATENATE 'and' w_dfies-fieldname 'in <fs_tab6>'(007) INTO w_where SEPARATED BY space.
      APPEND w_where TO t_where.

    WHEN 7.
* create work area of structure g_STRUCTDESCR -> g_WA
      CREATE DATA g_wa TYPE HANDLE g_structdescr.
      ASSIGN g_wa->* TO <fs_wa7>.
      MOVE-CORRESPONDING w_select TO <fs_wa7>.

* Create dynmaic internal table
      CREATE DATA g_tab TYPE HANDLE g_tabledescr.
      ASSIGN g_tab->* TO <fs_tab7>.
      APPEND <fs_wa7> TO <fs_tab7>.

      CONCATENATE 'and' w_dfies-fieldname 'in <fs_tab7>'(008) INTO w_where SEPARATED BY space.
      APPEND w_where TO t_where.

    WHEN 8.
* create work area of structure g_STRUCTDESCR -> g_WA
      CREATE DATA g_wa TYPE HANDLE g_structdescr.
      ASSIGN g_wa->* TO <fs_wa8>.
      MOVE-CORRESPONDING w_select TO <fs_wa8>.

* Create dynmaic internal table
      CREATE DATA g_tab TYPE HANDLE g_tabledescr.
      ASSIGN g_tab->* TO <fs_tab8>.
      APPEND <fs_wa8> TO <fs_tab8>.

      CONCATENATE 'and' w_dfies-fieldname 'in <fs_tab8>'(009) INTO w_where SEPARATED BY space.
      APPEND w_where TO t_where.

    WHEN 9.
* create work area of structure g_STRUCTDESCR -> g_WA
      CREATE DATA g_wa TYPE HANDLE g_structdescr.
      ASSIGN g_wa->* TO <fs_wa9>.
      MOVE-CORRESPONDING w_select TO <fs_wa9>.

* Create dynmaic internal table
      CREATE DATA g_tab TYPE HANDLE g_tabledescr.
      ASSIGN g_tab->* TO <fs_tab9>.
      APPEND <fs_wa9> TO <fs_tab9>.

      CONCATENATE 'and' w_dfies-fieldname 'in <fs_tab9>'(010) INTO w_where SEPARATED BY space.
      APPEND w_where TO t_where.

    WHEN 10.
* create work area of structure g_STRUCTDESCR -> g_WA
      CREATE DATA g_wa TYPE HANDLE g_structdescr.
      ASSIGN g_wa->* TO <fs_wa10>.
      MOVE-CORRESPONDING w_select TO <fs_wa10>.

* Create dynmaic internal table
      CREATE DATA g_tab TYPE HANDLE g_tabledescr.
      ASSIGN g_tab->* TO <fs_tab10>.
      APPEND <fs_wa10> TO <fs_tab10>.

      CONCATENATE 'and' w_dfies-fieldname 'in <fs_tab10>'(011) INTO w_where SEPARATED BY space.
      APPEND w_where TO t_where.

  ENDCASE.


ENDFORM.                    " DYNAMIC_WHERE_CLASS
*&---------------------------------------------------------------------*
*&      Form  FETCH_REQUIRED_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fetch_required_data .

  CHECK t_doc_ext[] IS NOT INITIAL.

*Start of Deletion MOD-001
*  SELECT
*          bukrs
*          belnr
*          gjahr
*          cpudt
*          cputm
*          xblnr
*          awtyp
*          awkey
*          monat
*    FROM bkpf
*    INTO TABLE t_bkpf
*    FOR ALL ENTRIES IN t_doc_ext
*    WHERE  bukrs = t_doc_ext-COMPANY_CODE
*      AND  belnr = t_doc_ext-FI_DOCUMENT_NO
*      AND  gjahr = t_doc_ext-FI_DOCUMENT_YEAR.
*
*
*
*  CHECK sy-subrc = 0.
*End of Deletion MOD-001

  SELECT
          COMPANY_CODE
          FI_DOCUMENT_NO
          FI_DOCUMENT_YEAR
          FI_DOCUMENT_LINE_NO
          LIV_DOCUMENT_LINE_NO
          WO_SETTLE_SEQ
          VERSION
          TAX_ELEMENT_NBR
          TAXABLE_STATE
          TAXABLE_COUNTY
          TAXABLE_CITY
          AUTHORITY_TYPE
          AMOUNT_AU
          BASIS_AU
          AUTHORITY_CURRENCY_CODE
    FROM /DS1/FI_MT_DPOUT
    INTO TABLE t_doc_out
    FOR ALL ENTRIES IN t_doc_ext
    WHERE  COMPANY_CODE = t_doc_ext-COMPANY_CODE
      AND  FI_DOCUMENT_NO = t_doc_ext-FI_DOCUMENT_NO
      AND  FI_DOCUMENT_YEAR = t_doc_ext-FI_DOCUMENT_YEAR
*      and FI_DOCUMENT_NO = '1500000052'
*     DEFECT 311650 /RT 6873
*      AND  FI_DOCUMENT_LINE_NO = t_doc_ext-FI_DOCUMENT_LINE_NO.
      AND  FI_DOCUMENT_LINE_NO = t_doc_ext-FI_DOCUMENT_LINE_NO
      AND PROCESS_STATUS_CALC in ('8','9').


  CHECK sy-subrc = 0.


ENDFORM.                    " FETCH_REQUIRED_DATA
*&---------------------------------------------------------------------*
*&      Form  REFRESH_GLOBAL_VARIABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_global_variables .

  CLEAR: w_ccode, w_data, w_where, w_select, w_dfies. " w_stamp.

  REFRESH: t_ccode[], t_data[], t_where[], t_dfies[].

  CLEAR: g_structdescr, g_tabledescr, g_datadescr, t_components,
         w_component, g_wa, g_tab, g_count, g_dpext, g_initial_call, g_s_if, g_packagesize.

  REFRESH: r_bukrs[], r_belnr[], r_gjahr[], r_cpudt[]. "r_stamp[].


  IF <fs_wa1> IS ASSIGNED.
    CLEAR: <fs_wa1>.
  ENDIF.

  IF <fs_wa2> IS ASSIGNED.
    CLEAR: <fs_wa2>.
  ENDIF.

  IF <fs_wa3> IS ASSIGNED.
    CLEAR: <fs_wa3>.
  ENDIF.

  IF <fs_wa4> IS ASSIGNED.
    CLEAR: <fs_wa4>.
  ENDIF.

  IF <fs_wa5> IS ASSIGNED.
    CLEAR: <fs_wa5>.
  ENDIF.

  IF <fs_wa6> IS ASSIGNED.
    CLEAR: <fs_wa6>.
  ENDIF.

  IF <fs_wa7> IS ASSIGNED.
    CLEAR: <fs_wa7>.
  ENDIF.

  IF <fs_wa8> IS ASSIGNED.
    CLEAR: <fs_wa8>.
  ENDIF.

  IF <fs_wa9> IS ASSIGNED.
    CLEAR: <fs_wa9>.
  ENDIF.

  IF <fs_wa10> IS ASSIGNED.
    CLEAR: <fs_wa10>.
  ENDIF.

  IF <fs_tab1> IS ASSIGNED.
    REFRESH: <fs_tab1>[].
  ENDIF.

  IF <fs_tab2> IS ASSIGNED.
    REFRESH: <fs_tab2>[].
  ENDIF.

  IF <fs_tab3> IS ASSIGNED.
    REFRESH: <fs_tab3>[].
  ENDIF.

  IF <fs_tab4> IS ASSIGNED.
    REFRESH: <fs_tab4>[].
  ENDIF.

  IF <fs_tab5> IS ASSIGNED.
    REFRESH: <fs_tab5>[].
  ENDIF.

  IF <fs_tab6> IS ASSIGNED.
    REFRESH: <fs_tab6>[].
  ENDIF.

  IF <fs_tab7> IS ASSIGNED.
    REFRESH: <fs_tab7>[].
  ENDIF.

  IF <fs_tab8> IS ASSIGNED.
    REFRESH: <fs_tab8>[].
  ENDIF.

  IF <fs_tab9> IS ASSIGNED.
    REFRESH: <fs_tab9>[].
  ENDIF.

  IF <fs_tab10> IS ASSIGNED.
    REFRESH: <fs_tab10>[].
  ENDIF.

ENDFORM.                    " REFRESH_GLOBAL_VARIABLES
