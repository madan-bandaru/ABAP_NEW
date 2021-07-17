************************************************************************
* 2012-05-21   smartShift project

************************************************************************

************************************************************************
*                         PROGRAM INFORMATION                          *
************************************************************************
* RIEF ID.......: FSSR980                                              *
* PROGRAM.......: /DS1/FI_N_BSPWF_REQ_FORMS                            *
* TITLE.........  BSP Workflow Requests Report                         *
* OWNER.........: Vellaippan Thirunavukkarasu                          *
* AUTHOR........: Chaitanya Sudabatula(INCSU4)                         *
* DATE WRITTEN..: 10th Jan 2011                                        *
* R/3 RELEASE...: R2.3 - IR MAY 2011                                   *
* REV TRAC......: 46243                                                *
* TRANSPORTNR...: D94K9A38TI                                           *
* COPIED FROM...: N/A                                                  *
*----------------------------------------------------------------------*
* PROGRAM FUNCTION: Include to hold all the subroutines for the BSP    *
*                   workflow requests report.                          *
*----------------------------------------------------------------------*
* PROGRAM TYPE.. Report                                                *
* DEV. CLASS.... /DS1/FSS_01                                           *
* LOGICAL DB.... N/A                                                   *
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_STDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_stdate.
  IF p_stdate < '20060101'.
    MESSAGE e014 WITH 'Start Date should not be prior to 01/01/2006'(004).
  ENDIF.
ENDFORM.                    " VALIDATE_STDATE
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_ENDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_endate .
  IF p_endate < '20060101'.
    MESSAGE e014 WITH 'End Date should not be prior to 01/01/2006'(005).
  ENDIF.

  IF p_stdate > p_endate.
    MESSAGE e014 WITH 'End Date should be after start date'(006).
  ENDIF.
ENDFORM.                    " VALIDATE_ENDATE
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_ROLEID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_roleid .
  IF NOT s_role[] IS INITIAL.
    LOOP AT s_role.
      IF NOT s_role-low IN r_roleid.
        MESSAGE e014 WITH 'Selected Role ID is not allowed'(007).
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " VALIDATE_ROLEID
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen .
  LOOP AT SCREEN.
    IF screen-group1 = 'SC1'.
      IF screen-name = 'S_REQSTA-LOW' OR
         screen-name = 'S_REQSTA-HIGH'.
        screen-active = space.
        screen-input = space.
        screen-output = space.
        MODIFY SCREEN.
        IF rb1 = c_x.
          REFRESH s_reqsta.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  ROLE_F4_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM role_f4_help CHANGING fp_role TYPE agr_name.
  DATA:lw_role LIKE LINE OF r_roleid,
       lt_roleid TYPE STANDARD TABLE OF x_roleid,
       lw_roleid TYPE x_roleid,
       lt_results TYPE STANDARD TABLE OF ddshretval, " results int
       lw_results TYPE ddshretval.                   " results type


  LOOP AT r_roleid INTO lw_role.
    lw_roleid-agr_name = lw_role-low.
    APPEND lw_roleid TO lt_roleid.
  ENDLOOP.
  SORT lt_roleid BY agr_name.
  DELETE ADJACENT DUPLICATES FROM lt_roleid COMPARING agr_name.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = fp_role
      value_org       = 'S'
    TABLES
      value_tab       = lt_roleid
      return_tab      = lt_results
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE lt_results INTO lw_results INDEX 1.
    IF sy-subrc EQ 0.
      fp_role = lw_results-fieldval.
    ENDIF.
  ENDIF.


ENDFORM.                    " ROLE_F4_HELP
*&---------------------------------------------------------------------*
*&      Form  DATAOBJ_F4_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_S_DATOB_LOW  text
*----------------------------------------------------------------------*
FORM dataobj_f4_help  CHANGING fp_datob TYPE /ds1/mdm_object.

  DATA:lw_dataobj LIKE LINE OF r_dataobj,
*       lt_dataobj TYPE STANDARD TABLE OF x_dataobj,
       lt_results TYPE STANDARD TABLE OF ddshretval, " results int
       lw_results TYPE ddshretval.                   " results type

  SELECT object
         description
         FROM /ds1/mdm_mt_obj
         INTO TABLE t_dataobj
         WHERE object IN r_dataobj.
  IF sy-subrc EQ 0.
    SORT t_dataobj BY object.
    DELETE ADJACENT DUPLICATES FROM t_dataobj COMPARING object.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'OBJECT'
      value_org       = 'S'
    TABLES
      value_tab       = t_dataobj
      return_tab      = lt_results
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE lt_results INTO lw_results INDEX 1.
    IF sy-subrc EQ 0.
      fp_datob = lw_results-fieldval.
    ENDIF.
  ENDIF.

ENDFORM.                    " DATAOBJ_F4_HELP
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_DATA_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_data_object .
  IF NOT s_datob[] IS INITIAL.
    LOOP AT s_datob.
      IF NOT s_datob-low IN r_dataobj.
        MESSAGE e014 WITH 'Selected Data Object is not allowed'(008).
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " VALIDATE_DATA_OBJECT
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_ORG_UNIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_org_unit .
  DATA:l_land1 TYPE land1.

  SELECT SINGLE land1 FROM t005
         INTO l_land1
         WHERE land1 IN s_orgunt.
  IF sy-subrc NE 0.
    MESSAGE e014 WITH 'Invalid Organization Unit'(009).
  ENDIF.

ENDFORM.                    " VALIDATE_ORG_UNIT
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_BUKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_bukrs .
  DATA:l_bukrs TYPE bukrs.

  SELECT SINGLE bukrs
         FROM t001
         INTO l_bukrs
         WHERE bukrs IN s_bukrs.
  IF sy-subrc NE 0.
    MESSAGE e014 WITH 'Invalid Company Code'(010).
  ENDIF.

ENDFORM.                    " VALIDATE_BUKRS
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_REQUEST_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_request_status .
  DATA:l_status TYPE sww_wistat.
  IF NOT s_reqsta[] IS INITIAL.
    SELECT SINGLE wi_status
           FROM swwstatus
           INTO l_status
           WHERE wi_status IN s_reqsta.
    IF sy-subrc NE 0.
      MESSAGE e014 WITH 'Invalid Request Status'(011).
    ENDIF.
  ENDIF.
ENDFORM.                    " VALIDATE_REQUEST_STATUS
*&---------------------------------------------------------------------*
*&      Form  GET_WFTEMPLATES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_wftemplates .
  DATA:lw_wftemplates TYPE x_wftemplates,
       lw_wftemp LIKE LINE OF r_wftemplates.
  SELECT object
         wf_task
         FROM /ds1/mdm_mt_wfob
         INTO TABLE t_wftemplates
         WHERE object IN s_datob.
  IF sy-subrc EQ 0.
    SORT t_wftemplates BY wf_task object.
    lw_wftemp-sign = c_i.
    lw_wftemp-option = c_eq.
    LOOP AT t_wftemplates INTO lw_wftemplates.
      lw_wftemp-low = lw_wftemplates-wf_task.
      APPEND lw_wftemp TO r_wftemplates.
      CLEAR lw_wftemp-low.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_WFTEMPLATES
*&---------------------------------------------------------------------*
*&      Form  GET_USERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_users .
  SELECT uname
         FROM agr_users
         INTO TABLE t_users
         WHERE agr_name IN s_role.
  IF sy-subrc EQ 0.
    SORT t_users BY uname.
    DELETE ADJACENT DUPLICATES FROM t_users COMPARING uname.
  ELSE.
    MESSAGE e014 WITH 'Selected Role does not have users assigned'(012).
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " GET_USERS
*&---------------------------------------------------------------------*
*&      Form  GET_WORKITEMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_workitems .

  SELECT wi_id
         wi_stat
         wi_cd
         wi_ct
         wi_aed
         wi_aagent
         wi_rh_task
         FROM swwwihead
         INTO TABLE t_wi
         FOR ALL ENTRIES IN t_users
         WHERE wi_cd BETWEEN p_stdate AND p_endate
           AND wi_rh_task IN r_wftemplates
           AND wi_aagent = t_users-uname
           AND wi_type = 'F'
           AND wi_stat IN s_reqsta.
  IF sy-subrc EQ 0.
    SORT t_wi BY wi_id.
    SELECT wi_id
           wi_stat
           wi_cd
           wi_ct
           wi_aed
           wi_aagent
           top_wi_id
           FROM swwwihead
*           INTO TABLE t_subwi
                 INTO TABLE t_allwi
           FOR ALL ENTRIES IN t_wi
           WHERE top_wi_id = t_wi-wi_id
             AND wi_type IN ('W' , 'F').
*Defect 206775
*             AND wi_stat IN s_reqsta.
*Defect 206775
    IF sy-subrc EQ 0.
*      SORT t_subwi BY top_wi_id wi_id.
      SORT t_allwi BY top_wi_id wi_id.
    ENDIF.
    t_subwi[] = t_allwi[].
    DELETE t_subwi WHERE wi_stat NOT IN s_reqsta[].
    DELETE t_allwi WHERE wi_aagent IS INITIAL.

  ELSE.
** Begin of Defect 194311
    SELECT wi_id
           wi_stat
           wi_cd
           wi_ct
           wi_aed
           wi_aagent
           wi_rh_task
           FROM swwwihead
           INTO TABLE t_wi
           WHERE wi_cd BETWEEN p_stdate AND p_endate
             AND wi_rh_task IN r_wftemplates
             AND wi_type IN ('W' , 'F').

    IF sy-subrc = 0.
      SORT t_wi BY wi_id.
    ELSE.
      MESSAGE i014 WITH 'No work items found for the selection criteria'(013).
      LEAVE LIST-PROCESSING.
    ENDIF.
*End of defect 194311
  ENDIF.

ENDFORM.                    " GET_WORKITEMS
*&---------------------------------------------------------------------*
*&      Form  GET_OTHERDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_wfcontdata.
  TYPES:BEGIN OF lx_hist,
         uname  TYPE uname,
         sent_to  TYPE xfeld,
         actioned_by  TYPE xfeld,
         action_time  TYPE uzeit,
         action_date  TYPE datum,
         req_status_start TYPE  /ds1/mdm_req_status_old,
         req_status_end  TYPE /ds1/mdm_req_status_new,
         read_flag TYPE flag,
        END OF lx_hist.
  DATA:lt_history TYPE TABLE OF /ds1/mdm_ms_wf_history,
       lt_notes TYPE TABLE OF /ds1/mdm_ms_req_note_wf,
       lv_wi_handle TYPE REF TO if_swf_run_wim_internal,
       l_cnt TYPE REF TO if_swf_cnt_container,
       lw_wfcontdata TYPE x_wfcontdata,
       l_bukrs TYPE bukrs,
       lw_history TYPE /ds1/mdm_ms_wf_history,
       lw_notes TYPE /ds1/mdm_ms_req_note_wf,
       l_init_status TYPE char2,
       l_tabix TYPE sy-tabix,
       l_tabix_rej  TYPE sy-tabix,
       l_key TYPE bapi1022_key,
       lw_coce TYPE bapi0012_ccinputlist,
       lw_gfr TYPE /ds1/zfrc_request,
       lw_skb1 TYPE skb1.

  LOOP AT t_wi INTO w_wi.
    lt_allwi[] = t_allwi[].
    DELETE lt_allwi WHERE top_wi_id NE w_wi-wi_id.
    SORT lt_allwi BY wi_id.


    TRY.
        CALL METHOD cl_swf_run_wim_factory=>find_by_wiid
          EXPORTING
            im_wiid     = w_wi-wi_id
          RECEIVING
            re_instance = lv_wi_handle.
      CATCH cx_swf_run_wim_def_failed
            cx_swf_run_wim_read_failed
            cx_swf_run_wim_enq_failed
            CX_SWF_RUN_WIM.           "Smart(M)2012-03-03
        CONTINUE.
    ENDTRY.
* Get the container
    TRY.
    l_cnt = lv_wi_handle->get_wi_container( ).
    CATCH CX_SWF_RUN_WIM_READ_FAILED .                      "Smart(M)2012-03-03
    ENDTRY.
*get req_notes internal table data from the container
    TRY .
        CALL METHOD l_cnt->if_swf_cnt_element_access_1~element_get_value
          EXPORTING
            name  = c_req_notes
          IMPORTING
            value = lt_notes.
      CATCH cx_swf_cnt_elem_not_found
            cx_swf_cnt_elem_type_conflict
            cx_swf_cnt_unit_type_conflict
            cx_swf_cnt_container.
*        CONTINUE.
    ENDTRY.

    TRY .
*get t_wf_history internal table data from the container
        CALL METHOD l_cnt->if_swf_cnt_element_access_1~element_get_value
          EXPORTING
            name  = c_t_wf_history
          IMPORTING
            value = lt_history.
      CATCH cx_swf_cnt_elem_not_found
            cx_swf_cnt_elem_type_conflict
            cx_swf_cnt_unit_type_conflict
            cx_swf_cnt_container.
        CONTINUE.
    ENDTRY.


    READ TABLE t_wftemplates INTO w_wftemplates WITH KEY
                                  wf_task = w_wi-wi_rh_task
                                  BINARY SEARCH.
    IF sy-subrc EQ 0.
*chaeck the data object type
      CASE w_wftemplates-object.
        WHEN c_alo.
*get the company code data from the container
          TRY.
              CALL METHOD l_cnt->if_swf_cnt_element_access_1~element_get_value
                EXPORTING
                  name  = c_comp_code
                IMPORTING
                  value = l_bukrs.
            CATCH cx_swf_cnt_elem_not_found
                  cx_swf_cnt_elem_type_conflict
                  cx_swf_cnt_unit_type_conflict
                  cx_swf_cnt_container.
              CONTINUE.
          ENDTRY.

          PERFORM populate_data TABLES lt_history
                                       lt_notes
                                 USING l_bukrs
                                       c_alo
                                       c_alo_rej_status.

        WHEN c_ast.

*get the company code data from the container
          TRY.
              CALL METHOD l_cnt->if_swf_cnt_element_access_1~element_get_value
                EXPORTING
                  name  = c_key
                IMPORTING
                  value = l_key.
            CATCH cx_swf_cnt_elem_not_found
                  cx_swf_cnt_elem_type_conflict
                  cx_swf_cnt_unit_type_conflict
                  cx_swf_cnt_container.
              CONTINUE.
          ENDTRY.

          PERFORM populate_data TABLES lt_history
                                       lt_notes
                                 USING l_key-companycode
                                       c_ast
                                       c_def_rejstatus.

        WHEN c_bnk.
          TRY.
*get the company code data from the container
              CALL METHOD l_cnt->if_swf_cnt_element_access_1~element_get_value
                EXPORTING
                  name  = c_wf_bukrs
                IMPORTING
                  value = l_bukrs.
            CATCH cx_swf_cnt_elem_not_found
                  cx_swf_cnt_elem_type_conflict
                  cx_swf_cnt_unit_type_conflict
                  cx_swf_cnt_container.
              CONTINUE.
          ENDTRY.

          PERFORM populate_data TABLES lt_history
                             lt_notes
                       USING l_bukrs
                             c_bnk
                             c_def_rejstatus.
        WHEN c_coc.
          TRY.
              CALL METHOD l_cnt->if_swf_cnt_element_access_1~element_get_value
                EXPORTING
                  name  = c_w_coce
                IMPORTING
                  value = lw_coce.
            CATCH cx_swf_cnt_elem_not_found
                  cx_swf_cnt_elem_type_conflict
                  cx_swf_cnt_unit_type_conflict
                  cx_swf_cnt_container.
              CONTINUE.
          ENDTRY.

          PERFORM populate_data TABLES lt_history
                             lt_notes
                       USING lw_coce-comp_code
                             c_coc
                             c_def_rejstatus.

        WHEN c_gfr.
          TRY.
              CALL METHOD l_cnt->if_swf_cnt_element_access_1~element_get_value
                EXPORTING
                  name  = c_ms_frce
                IMPORTING
                  value = lw_gfr.
            CATCH cx_swf_cnt_elem_not_found
                  cx_swf_cnt_elem_type_conflict
                  cx_swf_cnt_unit_type_conflict
                  cx_swf_cnt_container.
              CONTINUE.
          ENDTRY.

          PERFORM populate_data TABLES lt_history
                                       lt_notes
                                 USING lw_gfr-bukrs
                                       c_gfr
                                       c_def_rejstatus.
        WHEN c_gla.
          TRY.
              CALL METHOD l_cnt->if_swf_cnt_element_access_1~element_get_value
                EXPORTING
                  name  = c_skb1
                IMPORTING
                  value = lw_skb1.
            CATCH cx_swf_cnt_elem_not_found
                  cx_swf_cnt_elem_type_conflict
                  cx_swf_cnt_unit_type_conflict
                  cx_swf_cnt_container.
              CONTINUE.
          ENDTRY.

          PERFORM populate_data TABLES lt_history
                                       lt_notes
                                 USING lw_skb1-bukrs
                                       c_gla
                                       c_def_rejstatus.
        WHEN c_prc.
          TRY.
              CALL METHOD l_cnt->if_swf_cnt_element_access_1~element_get_value
                EXPORTING
                  name  = c_g_bukrs_routing
                IMPORTING
                  value = l_bukrs.
            CATCH cx_swf_cnt_elem_not_found
                  cx_swf_cnt_elem_type_conflict
                  cx_swf_cnt_unit_type_conflict
                  cx_swf_cnt_container.
              CONTINUE.
          ENDTRY.

          PERFORM populate_data TABLES lt_history
                                       lt_notes
                                 USING l_bukrs
                                       c_prc
                                       c_def_rejstatus.
        WHEN c_sce.

          TRY.
              CALL METHOD l_cnt->if_swf_cnt_element_access_1~element_get_value
                EXPORTING
                  name  = c_g_bukrs_routing
                IMPORTING
                  value = l_bukrs.
            CATCH cx_swf_cnt_elem_not_found
                  cx_swf_cnt_elem_type_conflict
                  cx_swf_cnt_unit_type_conflict
                  cx_swf_cnt_container.
              CONTINUE.
          ENDTRY.

          PERFORM populate_data TABLES lt_history
                                       lt_notes
                                 USING l_bukrs
                                       c_sce
                                       c_def_rejstatus.
      ENDCASE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_WFCONTDATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  TYPES:BEGIN OF lx_t001,
         bukrs TYPE bukrs,
         land1 TYPE land1,
        END OF lx_t001.
  DATA:lt_wfcontdata TYPE STANDARD TABLE OF x_wfcontdata,
       lt_wfcontdata_ret TYPE STANDARD TABLE OF x_wfcontdata,
       lt_t001 TYPE STANDARD TABLE OF lx_t001,
       lw_t001 TYPE lx_t001,
       lw_clopnrep TYPE x_clopnrep,
       lw_rejrep TYPE x_rejrept,
       lw_subwi TYPE x_subwi,
       lw_dataobj TYPE x_dataobj,
       lw_wi TYPE x_wi,
       l_new TYPE flag,
       l_validwi TYPE flag,
       lw_wfcontdata TYPE x_wfcontdata.
  FIELD-SYMBOLS:<lfs_subwi> TYPE x_subwi,
                 <lfs_wfcontdata> TYPE x_wfcontdata.

  lt_wfcontdata[] = t_wfcontdata[].
  SORT lt_wfcontdata BY bukrs.
  DELETE ADJACENT DUPLICATES FROM lt_wfcontdata COMPARING bukrs.

*get the country keys for all the company codes
  IF NOT lt_wfcontdata[] IS INITIAL.
    SELECT bukrs
           land1
           FROM t001
           INTO TABLE lt_t001
           FOR ALL ENTRIES IN lt_wfcontdata
           WHERE bukrs = lt_wfcontdata-bukrs.
  ENDIF.

  SORT t_wfcontdata BY wi_id taskid.

  IF rb1 = c_x.
    REFRESH lt_wfcontdata.
    LOOP AT t_wfcontdata ASSIGNING <lfs_wfcontdata> WHERE
                                   rejflag = c_x OR retflag = c_x.
      APPEND <lfs_wfcontdata> TO lt_wfcontdata.
    ENDLOOP.

    LOOP AT lt_wfcontdata ASSIGNING <lfs_wfcontdata>.
      IF <lfs_wfcontdata>-bukrs IN s_bukrs.
        READ TABLE t_wi INTO lw_wi WITH KEY wi_id = <lfs_wfcontdata>-wi_id
                                            BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_t001 INTO lw_t001 WITH KEY bukrs = <lfs_wfcontdata>-bukrs
                                                   BINARY SEARCH.
*check if the country is valid for the selection
          IF sy-subrc EQ 0 AND lw_t001-land1 IN s_orgunt.
*            READ TABLE t_users WITH KEY uname = <lfs_wfcontdata>-agent
*                                        BINARY SEARCH
*                                        TRANSPORTING NO FIELDS.
*            IF sy-subrc EQ 0.
            l_new = c_x.
            l_validwi = c_x.
            lw_rejrep-wi_id = <lfs_wfcontdata>-wi_id.
            lw_rejrep-bukrs = <lfs_wfcontdata>-bukrs.
            lw_rejrep-land1 = lw_t001-land1.
            lw_rejrep-rejres = <lfs_wfcontdata>-text.
            lw_rejrep-agent = <lfs_wfcontdata>-agent.
            READ TABLE t_dataobj INTO lw_dataobj WITH KEY object = <lfs_wfcontdata>-object
                                            BINARY SEARCH.
            IF sy-subrc EQ 0.
              lw_rejrep-obj_desc = lw_dataobj-description.
            ENDIF.
            IF lw_rejrep IS NOT INITIAL.
              APPEND lw_rejrep TO t_rejrep.
            ENDIF.
            CLEAR lw_rejrep.
*            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.


  IF rb2 = c_x.

    REFRESH lt_wfcontdata[].
    lt_wfcontdata[] = t_wfcontdata[].
    lt_wfcontdata_ret[] = t_wfcontdata[].
    DELETE lt_wfcontdata WHERE rejflag NE c_x.
    DELETE lt_wfcontdata_ret WHERE retflag NE c_x.

    LOOP AT t_wfcontdata ASSIGNING <lfs_wfcontdata>.
      CLEAR l_new.
      AT NEW wi_id.
        READ TABLE t_wi INTO lw_wi WITH KEY wi_id = <lfs_wfcontdata>-wi_id
                                            BINARY SEARCH.
        IF sy-subrc EQ 0 AND lw_wi-wi_stat IN s_reqsta.
*check if the work item is rejected,if not then this is a valid wi for the
**open and closed work items report.
*          READ TABLE lt_wfcontdata WITH KEY wi_id = <lfs_wfcontdata>-wi_id
*                                            taskid = <lfs_wfcontdata>-taskid
*                              BINARY SEARCH
*                              TRANSPORTING NO FIELDS.
*          IF sy-subrc NE 0.
*check if the work item is returned,if not then this is a valid wi for the
*open and closed work items report.
*            READ TABLE lt_wfcontdata_ret WITH KEY wi_id = <lfs_wfcontdata>-wi_id
*                                                  taskid = <lfs_wfcontdata>-taskid
*                                BINARY SEARCH
*                                TRANSPORTING NO FIELDS.
*            IF sy-subrc NE 0.
*check if the work item is having company code that is specified in the
*selection screem
          IF <lfs_wfcontdata>-bukrs IN s_bukrs.
            READ TABLE lt_t001 INTO lw_t001 WITH KEY bukrs = <lfs_wfcontdata>-bukrs
                                                     BINARY SEARCH.
*check if the country is valid for the selection
            IF sy-subrc EQ 0 AND lw_t001-land1 IN s_orgunt.
              l_new = c_x.
              l_validwi = c_x.
              lw_clopnrep-wi_id = <lfs_wfcontdata>-wi_id.
              lw_clopnrep-bukrs = <lfs_wfcontdata>-bukrs.
              lw_clopnrep-land1 = lw_t001-land1.
              lw_clopnrep-wi_stat = lw_wi-wi_stat.
              READ TABLE t_dataobj INTO lw_dataobj WITH KEY object = <lfs_wfcontdata>-object
                                              BINARY SEARCH.
              IF sy-subrc EQ 0.
                lw_clopnrep-obj_desc = lw_dataobj-description.
              ENDIF.
              IF lw_clopnrep IS NOT INITIAL.
                APPEND lw_clopnrep TO t_clopnrep.
              ENDIF.
              CLEAR lw_clopnrep.
            ENDIF.
          ENDIF.
*            ENDIF.
*          ENDIF.
        ENDIF.
      ENDAT.

      IF l_validwi = c_x.
*        AND <lfs_wfcontdata>-retflag IS INITIAL
*        AND <lfs_wfcontdata>-rejflag IS INITIAL.
        LOOP AT t_subwi ASSIGNING <lfs_subwi> WHERE top_wi_id = <lfs_wfcontdata>-wi_id
                                             AND wi_aagent = <lfs_wfcontdata>-agent
                                             AND read_wi NE c_x.
          lw_clopnrep-wi_cd = <lfs_subwi>-wi_cd.
          IF l_new = c_x.
            lw_clopnrep-wi_id = <lfs_wfcontdata>-wi_id.
            lw_clopnrep-bukrs = <lfs_wfcontdata>-bukrs.
            lw_clopnrep-land1 = lw_t001-land1.
            lw_clopnrep-obj_desc = lw_dataobj-description.
            lw_clopnrep-wi_stat = c_completed.
            lw_clopnrep-wi_aed = lw_wi-wi_cd.
            PERFORM no_of_days_calculate USING lw_clopnrep-wi_cd
                                   lw_clopnrep-wi_aed
                          CHANGING lw_clopnrep-nodays.
*            lw_clopnrep-nodays = lw_clopnrep-wi_aed - lw_clopnrep-wi_cd.
            IF lw_clopnrep-nodays LE 2.
              lw_clopnrep-porf = c_pass.
            ELSE.
              lw_clopnrep-porf = c_fail.
            ENDIF.
          ELSE.
            lw_clopnrep-wi_id = <lfs_wfcontdata>-wi_id.
            lw_clopnrep-bukrs = <lfs_wfcontdata>-bukrs.
            lw_clopnrep-land1 = lw_t001-land1.
            lw_clopnrep-obj_desc = lw_dataobj-description.
            lw_clopnrep-wi_stat = <lfs_subwi>-wi_stat.
            IF <lfs_subwi>-wi_aed NE c_99991231.
              lw_clopnrep-wi_aed = <lfs_subwi>-wi_aed.

              PERFORM no_of_days_calculate USING lw_clopnrep-wi_cd
                                                  lw_clopnrep-wi_aed
                                        CHANGING lw_clopnrep-nodays.
*              lw_clopnrep-nodays = lw_clopnrep-wi_aed - lw_clopnrep-wi_cd.
              IF lw_clopnrep-nodays LE 2.
                lw_clopnrep-porf = c_pass.
              ELSE.
                lw_clopnrep-porf = c_fail.
              ENDIF.
            ENDIF.
          ENDIF.
          <lfs_subwi>-read_wi = c_x.
          lw_clopnrep-agent = <lfs_subwi>-wi_aagent.
          EXIT.
        ENDLOOP.
        IF lw_clopnrep IS NOT INITIAL.
          APPEND lw_clopnrep TO t_clopnrep.
        ENDIF.
        CLEAR lw_clopnrep.
      ENDIF.

      AT END OF wi_id.
        CLEAR l_validwi.
      ENDAT.
    ENDLOOP.
  ENDIF.

  IF rb1 = c_x.
    IF t_rejrep[] IS INITIAL.
      MESSAGE i014 WITH 'No work items found for the selection criteria'(013).
      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSE.
    IF t_clopnrep[] IS INITIAL.
      MESSAGE i014 WITH 'No work items found for the selection criteria'(013).
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog .
  FIELD-SYMBOLS:<lfs_fcat_rejrep> TYPE slis_fieldcat_alv.

  IF rb1 = c_x.
    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_program_name         = sy-repid
        i_structure_name       = c_rejrep
        i_client_never_display = c_x
      CHANGING
        ct_fieldcat            = t_fieldcat_rejrep
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE i014 WITH 'Error in fieldcatalog details'(014).
      LEAVE LIST-PROCESSING.
    ENDIF.

    LOOP AT t_fieldcat_rejrep ASSIGNING <lfs_fcat_rejrep>.
      CASE <lfs_fcat_rejrep>-fieldname.
        WHEN c_wi_id.
          <lfs_fcat_rejrep>-seltext_s = 'WorkFlowID'.
          <lfs_fcat_rejrep>-seltext_m = 'WorkFlow ID'(016).
          <lfs_fcat_rejrep>-seltext_l = 'WorkFlow ID'(016).
          <lfs_fcat_rejrep>-outputlen = 15.
        WHEN c_bukrs.
          <lfs_fcat_rejrep>-seltext_s = 'Comp Code'.
          <lfs_fcat_rejrep>-seltext_m = 'Company Code'(017).
          <lfs_fcat_rejrep>-seltext_l = 'Company Code'(017).
          <lfs_fcat_rejrep>-outputlen = 10.
        WHEN c_land1.
          <lfs_fcat_rejrep>-seltext_s = 'Org Unt'.
          <lfs_fcat_rejrep>-seltext_m = 'Organization Unit'(018).
          <lfs_fcat_rejrep>-seltext_l = 'Organization Unit'(018).
          <lfs_fcat_rejrep>-outputlen = 7.
        WHEN c_obj_desc.
          <lfs_fcat_rejrep>-seltext_s = 'Data Obj'.
          <lfs_fcat_rejrep>-seltext_m = 'Data Object'(019).
          <lfs_fcat_rejrep>-seltext_l = 'Data Object'(019).
          <lfs_fcat_rejrep>-outputlen = 20.
        WHEN c_agent.
          <lfs_fcat_rejrep>-seltext_s = 'Rejected By Analyst Alias ID'(020).
          <lfs_fcat_rejrep>-seltext_m = 'Rejected By Analyst Alias ID'(020).
          <lfs_fcat_rejrep>-seltext_l = 'Rejected By Analyst Alias ID'(020).
          <lfs_fcat_rejrep>-outputlen = 30.
        WHEN c_rejres.
          <lfs_fcat_rejrep>-seltext_s = 'Rej. Reason'.
          <lfs_fcat_rejrep>-seltext_m = 'Rej./Ret. Reason'(021).
          <lfs_fcat_rejrep>-seltext_l = 'Rejected/Returned Reason'(022).
      ENDCASE.

    ENDLOOP.
  ELSE.
    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_program_name         = sy-repid
        i_structure_name       = c_clopnrep
        i_client_never_display = c_x
      CHANGING
        ct_fieldcat            = t_fieldcat_clopn
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE i014 WITH 'Error in fieldcatalog details'(014).
      LEAVE LIST-PROCESSING.
    ENDIF.

    LOOP AT t_fieldcat_clopn ASSIGNING <lfs_fcat_rejrep>.
      CASE <lfs_fcat_rejrep>-fieldname.
        WHEN c_wi_id.
          <lfs_fcat_rejrep>-seltext_s = 'WorkFlowID'(030).
          <lfs_fcat_rejrep>-seltext_m = 'WorkFlow ID'(016).
          <lfs_fcat_rejrep>-seltext_l = 'WorkFlow ID'(016).
          <lfs_fcat_rejrep>-outputlen = 15.
        WHEN c_bukrs.
          <lfs_fcat_rejrep>-seltext_s = 'Comp Code'(031).
          <lfs_fcat_rejrep>-seltext_m = 'Company Code'(017).
          <lfs_fcat_rejrep>-seltext_l = 'Company Code'(017).
          <lfs_fcat_rejrep>-outputlen = 10.
        WHEN c_land1.
          <lfs_fcat_rejrep>-seltext_s = 'Org Unt'(032).
          <lfs_fcat_rejrep>-seltext_m = 'Organization Unit'(018).
          <lfs_fcat_rejrep>-seltext_l = 'Organization Unit'(018).
          <lfs_fcat_rejrep>-outputlen = 7.
        WHEN c_obj_desc.
          <lfs_fcat_rejrep>-seltext_s = 'Data Obj'(033).
          <lfs_fcat_rejrep>-seltext_m = 'Data Object'(019).
          <lfs_fcat_rejrep>-seltext_l = 'Data Object'(019).
          <lfs_fcat_rejrep>-outputlen = 20.
        WHEN c_wi_stat.
          <lfs_fcat_rejrep>-seltext_s = 'WI Status'(034).
          <lfs_fcat_rejrep>-seltext_m = 'Status Of Request'(023).
          <lfs_fcat_rejrep>-seltext_l = 'Status Of Request'(023).
          <lfs_fcat_rejrep>-outputlen = 20.
        WHEN c_agent.
          <lfs_fcat_rejrep>-seltext_s = 'Alias ID'(035).
          <lfs_fcat_rejrep>-seltext_m = 'Analyst Alias ID'(024).
          <lfs_fcat_rejrep>-seltext_l = 'Analyst Alias ID'(024).
          <lfs_fcat_rejrep>-outputlen = 25.
        WHEN c_wi_cd.
          <lfs_fcat_rejrep>-seltext_s = 'WI Recv. Date'(036).
          <lfs_fcat_rejrep>-seltext_m = 'WI Received Date'(025).
          <lfs_fcat_rejrep>-seltext_l = 'WI Received Date'(025).
          <lfs_fcat_rejrep>-outputlen = 20.
        WHEN c_wi_aed.
          <lfs_fcat_rejrep>-seltext_s = 'WI Process Date'(037).
          <lfs_fcat_rejrep>-seltext_m = 'WI Processed Date'(026).
          <lfs_fcat_rejrep>-seltext_l = 'WI Processed Date'(026).
          <lfs_fcat_rejrep>-outputlen = 20.
        WHEN c_nodays.
          <lfs_fcat_rejrep>-seltext_s = 'No.of days'(038).
          <lfs_fcat_rejrep>-seltext_m = 'No. Of days'(029).
          <lfs_fcat_rejrep>-seltext_l = 'No. Of days taken'(028).
          <lfs_fcat_rejrep>-outputlen = 20.
        WHEN c_porf.
          <lfs_fcat_rejrep>-seltext_s = 'Passed/Failed'(027).
          <lfs_fcat_rejrep>-seltext_m = 'Passed/Failed'(027).
          <lfs_fcat_rejrep>-seltext_l = 'Passed/Failed'(027).
          <lfs_fcat_rejrep>-outputlen = 15.
      ENDCASE.

    ENDLOOP.
  ENDIF.
ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .
  DATA:c_end_of_list TYPE slis_formname VALUE 'END_OF_LIST'.
  IF rb1 = c_x.
*    IF p_list = c_x.
*      CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
*        EXPORTING
*          i_callback_program = sy-repid
*          it_fieldcat        = t_fieldcat_rejrep
*          it_events          = t_events
*        TABLES
*          t_outtab           = t_rejrep
*        EXCEPTIONS
*          program_error      = 1.
*
*      IF sy-subrc <> 0.
*        MESSAGE i014(/ds1/a) WITH 'Error in list display'(015).
*        LEAVE LIST-PROCESSING.
*      ENDIF.
*    ELSE.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program          = sy-repid
        it_fieldcat                 = t_fieldcat_rejrep
        it_events                   = t_events
        i_callback_html_end_of_list = c_end_of_list
      TABLES
        t_outtab                    = t_rejrep
      EXCEPTIONS
        program_error               = 1.

    IF sy-subrc <> 0.
      MESSAGE i014(/ds1/a) WITH 'Error in list display'(015).
      LEAVE LIST-PROCESSING.
    ENDIF.
*    ENDIF.
  ENDIF.

  IF rb2 = c_x.
*    IF p_list = c_x.
*      CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
*        EXPORTING
*          i_callback_program = sy-repid
*          it_fieldcat        = t_fieldcat_clopn
*          it_events          = t_events
*        TABLES
*          t_outtab           = t_clopnrep
*        EXCEPTIONS
*          program_error      = 1.
*
*      IF sy-subrc <> 0.
*        MESSAGE i014(/ds1/a) WITH 'Error in list display'(015).
*        LEAVE LIST-PROCESSING.
*      ENDIF.
*    ELSE.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program          = sy-repid
        it_fieldcat                 = t_fieldcat_clopn
        it_events                   = t_events
        i_callback_html_end_of_list = c_end_of_list
      TABLES
        t_outtab                    = t_clopnrep
      EXCEPTIONS
        program_error               = 1.

    IF sy-subrc <> 0.
      MESSAGE i014(/ds1/a) WITH 'Error in list display'(015).
      LEAVE LIST-PROCESSING.
    ENDIF.
*    ENDIF.
  ENDIF.

  REFRESH:t_clopnrep,
          t_rejrep.
ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  GET_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_object .
  IF t_dataobj[] IS INITIAL.
    SELECT object
           description
           FROM /ds1/mdm_mt_obj
           INTO TABLE t_dataobj
           WHERE object IN r_dataobj.
    IF sy-subrc EQ 0.
      SORT t_dataobj BY object.
      DELETE ADJACENT DUPLICATES FROM t_dataobj COMPARING object.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_OBJECT
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM top_of_page.

  DATA: lt_listhead TYPE slis_t_listheader WITH HEADER LINE,
        lt_listhead1 TYPE slis_t_listheader WITH HEADER LINE,
        lw_headerreport TYPE REPID,              "smart: 2012-05-21 #166
        lw_header TYPE sy-title,
        lw_header_list TYPE string,
        l_info TYPE slis_entry,
        lt_clopnrep TYPE STANDARD TABLE OF /ds1/fi_ms_fcat_clopnrep,
        lt_rejrep TYPE STANDARD TABLE OF /ds1/fi_ms_fcat_rejrep,
        l_lines TYPE int4,
        lw_listhead TYPE slis_listheader,
        l_lineschar TYPE char10.


  FIELD-SYMBOLS:<lfs_listhead> TYPE slis_listheader.

  lw_headerreport = sy-repid.
  lw_header = 'BSP Workflow Requests Monitoring Report'.
  lw_header_list = 'BSP Workflow Requests Monitoring Report'.

  CALL FUNCTION '/DS1/C_ALVGRIDHEADER'
    EXPORTING
      user_name          = sy-uname
      report_name        = lw_headerreport
      date               = sy-datum
      time               = sy-uzeit
      header_text        = lw_header
      selection_screen   = 'X'
    TABLES
      header             = lt_listhead
    EXCEPTIONS
      no_selectionscreen = 1
      no_report          = 2
      OTHERS             = 3.
  CASE sy-subrc.
    WHEN 1.
      MESSAGE e014 WITH 'NO_SELECTIONSCREEN'(011).
    WHEN 2.
      MESSAGE e014 WITH 'NO_REPORT'(012).
    WHEN 3.
      MESSAGE e014 WITH 'OTHERS'(013).
  ENDCASE.


  IF rb1 = c_x.

    PERFORM change_header USING lt_listhead[]
                          CHANGING lt_listhead1[].
  ELSE.

    PERFORM change_header USING lt_listhead[]
                          CHANGING lt_listhead1[].
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_listhead1[].
ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  ALV_EVENTS_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_events_get .

  DATA: ls_event TYPE slis_alv_event.  "ALV EVENTS

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 0
    IMPORTING
      et_events       = t_events
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    SORT t_events BY name.
    READ TABLE t_events INTO ls_event
          WITH KEY name =  slis_ev_top_of_page
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      MOVE slis_ev_top_of_page TO ls_event-form.
      MODIFY t_events FROM ls_event TRANSPORTING form
               WHERE name = slis_ev_top_of_page.
      CLEAR ls_event.
    ENDIF.
  ENDIF.

ENDFORM.                    " ALV_EVENTS_GET
*&---------------------------------------------------------------------*
*&      Form  POPULATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_HISTORY  text
*      -->P_LT_NOTES  text
*      -->P_C_DEF_REJSTATUS  text
*      -->P_LW_WFCONTDATA  text
*----------------------------------------------------------------------*
FORM populate_data  TABLES   lt_history TYPE /ds1/mdm_tt_wf_history
                             lt_notes TYPE /ds1/mdm_tt_req_note_wf
                    USING    l_bukrs TYPE bukrs
                             fp_dataobj TYPE /ds1/mdm_object
                             fp_rejstatus TYPE char2.


  DATA:lw_history TYPE /ds1/mdm_ms_wf_history,
       lw_notes TYPE /ds1/mdm_ms_req_note_wf,
       l_init_status TYPE char2,
       l_init_user TYPE uname,
       l_tabix TYPE sy-tabix,
       l_tabix_rej  TYPE sy-tabix,
       lw_wfcontdata TYPE x_wfcontdata,
       lt_history_tmp TYPE STANDARD TABLE OF /ds1/mdm_ms_wf_history,
       l_prev_status TYPE  char2,
       l_prev_user TYPE uname,
       l_tabix1 TYPE sy-tabix.

  FIELD-SYMBOLS:<lfs_history> TYPE /ds1/mdm_ms_wf_history.


  IF rb2 = c_x.
    READ TABLE lt_history WITH KEY req_status_end = fp_rejstatus
                                   TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      EXIT.
    ENDIF.
  ENDIF.

  IF fp_dataobj = c_gfr.
    SORT lt_notes BY creation_date creation_time note_id.
  ELSE.
    SORT lt_notes BY note_id DESCENDING.
  ENDIF.

  lw_wfcontdata-object = fp_dataobj.
  lw_wfcontdata-wi_id = w_wi-wi_id.
  lw_wfcontdata-wi_rh_task = w_wi-wi_rh_task.
  lw_wfcontdata-bukrs = l_bukrs.

*get the initial status of the work item.this is the basis for us
*to determine the returned work items.
*  READ TABLE lt_history INTO lw_history INDEX 1.
*  IF sy-subrc EQ 0.
*    l_init_status = lw_history-req_status_start.
*    CLEAR lw_history.
*  ENDIF.




  LOOP AT lt_history ASSIGNING <lfs_history>.
    l_tabix = sy-tabix.
    l_tabix1 = sy-tabix - 1.

    READ TABLE lt_allwi INTO lw_allwi INDEX l_tabix.
    IF sy-subrc EQ 0.
      lw_wfcontdata-taskid = lw_allwi-wi_id.
    ENDIF.

    AT FIRST.
      l_init_status = <lfs_history>-req_status_start.
      l_init_user = <lfs_history>-uname.
    ENDAT.


*    lw_wfcontdata-note_id = lw_notes-note_id.
    lw_wfcontdata-creation_date = <lfs_history>-action_date.
    lw_wfcontdata-agent = <lfs_history>-uname.
    lw_wfcontdata-creation_time = <lfs_history>-action_time.

    IF lt_notes[] IS NOT INITIAL.
      READ TABLE lt_notes INTO lw_notes INDEX l_tabix.
      IF sy-subrc EQ 0.
        lw_wfcontdata-text = lw_notes-text.
      ENDIF.
    ENDIF.
    IF  fp_rejstatus EQ <lfs_history>-req_status_end.
      lw_wfcontdata-rejflag = c_x.
    ENDIF.
    IF l_tabix1 >= 1.
      READ TABLE lt_history INTO lw_history INDEX l_tabix1.
      IF sy-subrc EQ 0.
        l_prev_status = lw_history-req_status_start.
      ENDIF.
    ENDIF.
**identify the rejected work item and the agent based on the index that we determined
**above
*    IF l_tabix = l_tabix_rej.
*      lw_wfcontdata-rejflag = c_x.
*    ENDIF.

*identify if any work item is with initial status in the req_status_end field
*this means that this work item is returned.
*    READ TABLE lt_history INTO lw_history INDEX l_tabix.
*    IF sy-subrc EQ 0 AND
    IF <lfs_history>-req_status_end = l_init_status AND
       <lfs_history>-uname NE l_init_user.
      lw_wfcontdata-retflag = c_x.
    ENDIF.

    IF <lfs_history>-req_status_end = l_prev_status AND
       <lfs_history>-uname NE l_init_user AND
       <lfs_history>-uname NE l_prev_user.
      lw_wfcontdata-retflag = c_x.
    ENDIF.

    l_prev_user = lw_history-uname.
    READ TABLE t_users WITH KEY uname = <lfs_history>-uname
                                  BINARY SEARCH
                                  TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      CLEAR :lw_wfcontdata-note_id,
             lw_wfcontdata-creation_date,
             lw_wfcontdata-text,
             lw_wfcontdata-agent,
             lw_wfcontdata-creation_time,
             lw_wfcontdata-rejflag,
             lw_wfcontdata-retflag.
      CONTINUE.
    ENDIF.

    APPEND lw_wfcontdata TO t_wfcontdata.
    CLEAR :lw_wfcontdata-note_id,
           lw_wfcontdata-creation_date,
           lw_wfcontdata-text,
           lw_wfcontdata-agent,
           lw_wfcontdata-creation_time,
           lw_wfcontdata-rejflag,
           lw_wfcontdata-retflag.
  ENDLOOP.

**ger the index of the work item which is rejected
*  READ TABLE lt_history INTO lw_history WITH KEY
*                                         req_status_end = fp_rejstatus.
*  IF sy-subrc EQ 0.
*    l_tabix_rej = sy-tabix.
*    CLEAR lw_history.
*  ENDIF.
*
*  SORT lt_notes BY note_id DESCENDING.
*  LOOP AT lt_notes INTO lw_notes.
*    l_tabix = sy-tabix.
*    READ TABLE t_users WITH KEY uname = lw_notes-author
*                                BINARY SEARCH
*                                TRANSPORTING NO FIELDS.
*    IF sy-subrc NE 0.
*      CONTINUE.
*    ENDIF.
*    lw_wfcontdata-note_id = lw_notes-note_id.
*    lw_wfcontdata-creation_date = lw_notes-creation_date.
*    lw_wfcontdata-text = lw_notes-text.
*    lw_wfcontdata-agent = lw_notes-author.
*    lw_wfcontdata-creation_time = lw_notes-creation_time.
*
*
*
**identify the rejected work item and the agent based on the index that we determined
**above
*    IF l_tabix = l_tabix_rej.
*      lw_wfcontdata-rejflag = c_x.
*    ENDIF.
*
**identify if any work item is with initial status in the req_status_end field
**this means that this work item is returned.
*    READ TABLE lt_history INTO lw_history INDEX l_tabix.
*    IF sy-subrc EQ 0 AND lw_history-req_status_end = l_init_status.
*      lw_wfcontdata-retflag = c_x.
*    ENDIF.
*
*    APPEND lw_wfcontdata TO t_wfcontdata.
*    CLEAR :lw_wfcontdata-note_id,
*           lw_wfcontdata-creation_date,
*           lw_wfcontdata-text,
*           lw_wfcontdata-agent,
*           lw_wfcontdata-creation_time,
*           lw_wfcontdata-rejflag,
*           lw_wfcontdata-retflag.
*  ENDLOOP.

ENDFORM.                    " POPULATE_DATA
*&---------------------------------------------------------------------*
*&      Form  CHANGE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_LISTHEAD  text
*      -->P_RB1  text
*      <--P_LT_LISTHEAD1  text
*----------------------------------------------------------------------*
FORM change_header  USING    lt_listhead TYPE slis_t_listheader
                    CHANGING lt_listhead1 TYPE slis_t_listheader.
  DATA:l_first TYPE flag1,
       lw_dataobj TYPE x_dataobj,
       l_strlen TYPE int4.
  FIELD-SYMBOLS:<lfs_listhead> TYPE slis_listheader.

  READ TABLE lt_listhead ASSIGNING <lfs_listhead> WITH KEY key = 'USER NAME'.
  IF sy-subrc EQ 0.
    <lfs_listhead>-key = 'Initiator Alias ID'.  " Defect # 194311
    APPEND <lfs_listhead> TO lt_listhead1.
  ENDIF.

  READ TABLE lt_listhead ASSIGNING <lfs_listhead> WITH KEY key = 'REPORT NAME'.
  IF sy-subrc EQ 0.
    IF rb1 = c_x.
      <lfs_listhead>-key = 'Report Name'.
      <lfs_listhead>-info = ': Report on number of requests Sent Back/rejected'.
      APPEND <lfs_listhead> TO lt_listhead1.
    ELSE.
      <lfs_listhead>-key = 'Report Name'.
      <lfs_listhead>-info = ': Report on No.of Req.Clo/open in each OU for each data objs'.
      APPEND <lfs_listhead> TO lt_listhead1.
    ENDIF.
  ENDIF.

  READ TABLE lt_listhead ASSIGNING <lfs_listhead> WITH KEY key = 'DATE'.
  IF sy-subrc EQ 0.
    <lfs_listhead>-key = 'Date'.
    APPEND <lfs_listhead> TO lt_listhead1.
  ENDIF.

  READ TABLE lt_listhead ASSIGNING <lfs_listhead> WITH KEY key = 'TIME'.
  IF sy-subrc EQ 0.
    <lfs_listhead>-key = 'Time'.
    APPEND <lfs_listhead> TO lt_listhead1.
  ENDIF.

*  READ TABLE lt_listhead ASSIGNING <lfs_listhead> WITH KEY = ''.
*  IF sy-subrc EQ 0.
  CLEAR <lfs_listhead>.
  <lfs_listhead>-typ = 'S'.
  <lfs_listhead>-info = c_dot.
  APPEND <lfs_listhead> TO lt_listhead1.
*  ENDIF.

  READ TABLE lt_listhead ASSIGNING <lfs_listhead> WITH KEY key = 'Start Date'.
  IF sy-subrc EQ 0.
    APPEND <lfs_listhead> TO lt_listhead1.
  ENDIF.

  READ TABLE lt_listhead ASSIGNING <lfs_listhead> WITH KEY key = 'End Date'.
  IF sy-subrc EQ 0.
    APPEND <lfs_listhead> TO lt_listhead1.
  ENDIF.

  LOOP AT lt_listhead ASSIGNING <lfs_listhead> WHERE key = 'FSS Role ID'.
    IF l_first = space.
      l_first = c_x.
      APPEND <lfs_listhead> TO lt_listhead1.
    ELSE.
      <lfs_listhead>-key = space.
      APPEND <lfs_listhead> TO lt_listhead1.
    ENDIF.
  ENDLOOP.
  CLEAR l_first.

  LOOP AT lt_listhead ASSIGNING <lfs_listhead> WHERE key = 'Data Objects'.
    IF l_first = space.
      l_first = c_x.
      REPLACE ALL OCCURRENCES OF c_dot IN <lfs_listhead>-info WITH space.
      REPLACE ALL OCCURRENCES OF c_slash IN <lfs_listhead>-info WITH space.
      REPLACE ALL OCCURRENCES OF c_colon IN <lfs_listhead>-info WITH space.
      CONDENSE <lfs_listhead>-info.
      READ TABLE t_dataobj INTO lw_dataobj WITH KEY object = <lfs_listhead>-info
                                                BINARY SEARCH.
      IF sy-subrc EQ 0.
        CONCATENATE  c_colon
                     <lfs_listhead>-info
                     c_hypen
                     lw_dataobj-description
                     INTO <lfs_listhead>-info
                     SEPARATED BY space.
      ENDIF.
      APPEND <lfs_listhead> TO lt_listhead1.
    ELSE.
      REPLACE ALL OCCURRENCES OF c_dot IN <lfs_listhead>-info WITH space.
      REPLACE ALL OCCURRENCES OF c_slash IN <lfs_listhead>-info WITH space.
      REPLACE ALL OCCURRENCES OF c_colon IN <lfs_listhead>-info WITH space.
      CONDENSE <lfs_listhead>-info.
      READ TABLE t_dataobj INTO lw_dataobj WITH KEY object = <lfs_listhead>-info
                                                BINARY SEARCH.
      IF sy-subrc EQ 0.
        CONCATENATE  c_colon
                     <lfs_listhead>-info
                     c_hypen
                     lw_dataobj-description
                     INTO <lfs_listhead>-info
                     SEPARATED BY space.
      ENDIF.
      <lfs_listhead>-key = space.
      APPEND <lfs_listhead> TO lt_listhead1.
    ENDIF.
  ENDLOOP.
  CLEAR l_first.

  LOOP AT lt_listhead ASSIGNING <lfs_listhead> WHERE key = 'Organization Unit'.
    IF l_first = space.
      l_first = c_x.
      APPEND <lfs_listhead> TO lt_listhead1.
    ELSE.
      <lfs_listhead>-key = space.
      APPEND <lfs_listhead> TO lt_listhead1.
    ENDIF.
  ENDLOOP.
  CLEAR l_first.

  LOOP AT lt_listhead ASSIGNING <lfs_listhead> WHERE key = 'Company Code'.
    IF l_first = space.
      l_first = c_x.
      APPEND <lfs_listhead> TO lt_listhead1.
    ELSE.
      <lfs_listhead>-key = space.
      APPEND <lfs_listhead> TO lt_listhead1.
    ENDIF.
  ENDLOOP.
  CLEAR l_first.

  IF rb1 NE c_x.
    LOOP AT lt_listhead ASSIGNING <lfs_listhead> WHERE key = 'Status of Request'.
      IF l_first = space.
        l_first = c_x.
        APPEND <lfs_listhead> TO lt_listhead1.
      ELSE.
        <lfs_listhead>-key = space.
        APPEND <lfs_listhead> TO lt_listhead1.
      ENDIF.
    ENDLOOP.
  ENDIF.
  CLEAR l_first.

*  IF p_grid = c_x.
*    READ TABLE lt_listhead ASSIGNING <lfs_listhead> WITH KEY key = 'ALV Grid Display'.
*    IF sy-subrc EQ 0.
*      APPEND <lfs_listhead> TO lt_listhead1.
*    ENDIF.
*  ELSE.
*    READ TABLE lt_listhead ASSIGNING <lfs_listhead> WITH KEY key = 'ALV List Display'.
*    IF sy-subrc EQ 0.
*      APPEND <lfs_listhead> TO lt_listhead1.
*    ENDIF.
*  ENDIF.


ENDFORM.                    " CHANGE_HEADER
*&---------------------------------------------------------------------*
*&      Form  end_of_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM end_of_list USING end TYPE REF TO cl_dd_document.

  DATA: ls_text TYPE sdydo_text_element.
  DATA:lt_clopnrep TYPE STANDARD TABLE OF /ds1/fi_ms_fcat_clopnrep,
       lt_rejrep TYPE STANDARD TABLE OF /ds1/fi_ms_fcat_rejrep,
       l_lines TYPE int4,
       l_lineschar TYPE char10,
       l_info TYPE slis_entry,
       lt_listhead1 TYPE slis_t_listheader,
       lw_listhead TYPE slis_listheader.
*  CALL METHOD end->new_line.
  IF rb1 = c_x.
    lt_rejrep[] = t_rejrep[].
    SORT lt_rejrep[] BY wi_id.
    DELETE ADJACENT DUPLICATES FROM lt_rejrep COMPARING wi_id.
    DESCRIBE TABLE lt_rejrep LINES l_lines.
    l_lineschar = l_lines.
    CONCATENATE 'Total Requests Sent Back /Rejected'
                 c_colon
                 l_lineschar
                 INTO ls_text
                 SEPARATED BY space.
    CALL METHOD end->add_text
      EXPORTING
        text = ls_text.
  ELSE.
    lt_clopnrep[] = t_clopnrep[].
    SORT lt_clopnrep[] BY wi_id.
    DELETE ADJACENT DUPLICATES FROM lt_clopnrep COMPARING wi_id.
    DESCRIBE TABLE lt_clopnrep LINES l_lines.
    l_lineschar = l_lines.
    CONCATENATE 'Total Requests Open/Closed'
                 c_colon
                 l_lineschar
                 INTO ls_text
                 SEPARATED BY space.
    CALL METHOD end->add_text
      EXPORTING
        text = ls_text.
  ENDIF.

*  IF rb1 = c_x.
*    lt_rejrep[] = t_rejrep[].
*    SORT lt_rejrep[] BY wi_id.
*    DELETE ADJACENT DUPLICATES FROM lt_rejrep COMPARING wi_id.
*    DESCRIBE TABLE lt_rejrep LINES l_lines.
*    l_lineschar = l_lines.
*    lw_listhead-typ = 'S'.
*    lw_listhead-key = 'Reqsts Retrnd/Rejected'.
*    CONCATENATE c_colon l_lineschar INTO l_info SEPARATED BY space.
*    lw_listhead-info = l_info.
*    APPEND lw_listhead TO lt_listhead1.
*  ELSE.
*    lt_clopnrep[] = t_clopnrep[].
*    SORT lt_clopnrep[] BY wi_id.
*    DELETE ADJACENT DUPLICATES FROM lt_clopnrep COMPARING wi_id.
*    DESCRIBE TABLE lt_clopnrep LINES l_lines.
*    l_lineschar = l_lines.
*    lw_listhead-typ = 'S'.
*    lw_listhead-key = 'Requests Open/Closed'.
*    CONCATENATE c_colon l_lineschar INTO l_info SEPARATED BY space.
*    lw_listhead-info = l_info.
*    APPEND lw_listhead TO lt_listhead1.
*  ENDIF.
*
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
*      it_list_commentary       = lt_listhead1
**     I_LOGO                   =
**     I_END_OF_LIST_GRID       =
*            .

ENDFORM.                    "end_of_list
*&---------------------------------------------------------------------*
*&      Form  NO_OF_DAYS_CALCULATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_CLOPNREP_WI_AED  text
*      -->P_LW_CLOPNREP_WI_CD  text
*      <--P_LW_CLOPNREP_NODAYS  text
*      <--P_IF  text
*      <--P_LW_CLOPNREP_NODAYS  text
*      <--P_LE  text
*      <--P_2  text
*----------------------------------------------------------------------*
FORM no_of_days_calculate  USING    fp_lw_clopnrep_wi_cd TYPE sww_cd
                                    fp_lw_clopnrep_wi_aed TYPE sww_aed
                           CHANGING fp_lw_clopnrep_nodays TYPE int4.
  DATA:l_wircvday TYPE p,
       l_wiactday TYPE p,
       l_chkdt TYPE datum.

  l_chkdt = fp_lw_clopnrep_wi_cd.

  IF fp_lw_clopnrep_wi_cd NE fp_lw_clopnrep_wi_aed.
    DO.
      CALL FUNCTION 'DAY_IN_WEEK'
        EXPORTING
          datum = l_chkdt
        IMPORTING
          wotnr = l_wircvday.
      IF l_wircvday LE 5.
        fp_lw_clopnrep_nodays = fp_lw_clopnrep_nodays + 1.
      ENDIF.
      l_chkdt = l_chkdt + 1.
      IF l_chkdt GT fp_lw_clopnrep_wi_aed.
        EXIT.
      ENDIF.
    ENDDO.
    fp_lw_clopnrep_nodays = fp_lw_clopnrep_nodays - 1.
  ELSE.
    fp_lw_clopnrep_nodays = 0.
  ENDIF.
ENDFORM.                    " NO_OF_DAYS_CALCULATE
