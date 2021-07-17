************************************************************************
*                         PROGRAM INFORMATION                          *
************************************************************************
* RIEF ID.......: FSSR980                                              *
* PROGRAM.......: /DS1/FI_CE_REPT980_BSPWF_REQ                         *
* TITLE.........  BSP Workflow Requests Report                         *
* OWNER.........: Vellaippan Thirunavukkarasu                          *
* AUTHOR........: Chaitanya Sudabatula(INCSU4)                         *
* DATE WRITTEN..: 10th Jan 2011                                        *
* R/3 RELEASE...: R2.3 - IR MAY 2011                                   *
* REV TRAC......: 46243                                                *
* TRANSPORTNR...: D94K9A38TI                                           *
* COPIED FROM...: N/A                                                  *
*----------------------------------------------------------------------*
* PROGRAM FUNCTION: This program is used to display a report of all the*
*                   BSP workflow requests created/executed based on the*
*                   selection criteria.                                *
*----------------------------------------------------------------------*
* PROGRAM TYPE.. Report                                                *
* DEV. CLASS.... /DS1/FSS_01                                           *
* LOGICAL DB.... N/A                                                   *
************************************************************************

************************************************************************
* 2012-05-21   smartShift project
* Rule ids applied: #105 #166
************************************************************************

REPORT  /ds1/fi_ce_rept980_bspwf_req NO STANDARD PAGE HEADING
                                     LINE-SIZE 255
                                     LINE-COUNT 50
                                     MESSAGE-ID /ds1/a.

INCLUDE /ds1/bc_n_initialize_variable.

INCLUDE ZFI_N_BSPWF_REQ_TOP.
*INCLUDE /ds1/fi_n_bspwf_req_top.

INCLUDE ZFI_N_BSPWF_REQ_FORMS.
*INCLUDE /ds1/fi_n_bspwf_req_forms.

INITIALIZATION.
  initialize_variables_prog c_buss_ind c_prog.

AT SELECTION-SCREEN ON p_stdate.
  PERFORM validate_stdate.

AT SELECTION-SCREEN ON p_endate.
  PERFORM validate_endate.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_role-low.
  PERFORM role_f4_help CHANGING s_role-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_role-high.
  PERFORM role_f4_help CHANGING s_role-high.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_datob-low.
  PERFORM dataobj_f4_help CHANGING s_datob-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_datob-high.
  PERFORM dataobj_f4_help CHANGING s_datob-high.

AT SELECTION-SCREEN ON s_role.
  PERFORM validate_roleid.

AT SELECTION-SCREEN ON s_datob.
  PERFORM validate_data_object.

AT SELECTION-SCREEN ON s_orgunt.
  PERFORM validate_org_unit.

AT SELECTION-SCREEN ON s_bukrs.
  PERFORM validate_bukrs.

AT SELECTION-SCREEN ON s_reqsta.
  PERFORM validate_request_status.

AT SELECTION-SCREEN OUTPUT.
  IF rb1 = c_x.
    PERFORM modify_screen.
  ENDIF.

START-OF-SELECTION.
*Authorization check
  INCLUDE /ds1/utl_n_authorization.

  PERFORM get_object.

  PERFORM get_wftemplates.

  PERFORM get_users.

  PERFORM get_workitems.

  PERFORM get_wfcontdata.

  PERFORM process_data.

  PERFORM build_fieldcatalog.

  PERFORM alv_events_get.

  PERFORM display_alv.
