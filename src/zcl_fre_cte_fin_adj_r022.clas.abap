class ZCL_FRE_CTE_FIN_ADJ_R022 definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_CTE_FIN_POST_ADJ_DOC .
  interfaces IF_BADI_INTERFACE .

  types:
    BEGIN OF x_w_fcfg,
        bukrs     TYPE /ds1/cte_mt_fcfg-bukrs,
        item_type TYPE /ds1/cte_mt_fcfg-item_type,
        sgtxt_fid TYPE /ds1/cte_mt_fcfg-sgtxt_fid,
        zuonr_fid TYPE /ds1/cte_mt_fcfg-zuonr_fid,
        exists    TYPE boole_d,
      END OF x_w_fcfg .
  types:
    x_t_fcfg TYPE SORTED TABLE OF x_w_fcfg
          WITH UNIQUE KEY primary_key COMPONENTS bukrs item_type .
  types:
    BEGIN OF x_w_prps,
        pspnr	TYPE ps_posnr,
        prctr TYPE prctr,
      END OF x_w_prps .
  types:
    x_t_prps TYPE SORTED TABLE OF x_w_prps WITH UNIQUE KEY primary_key COMPONENTS pspnr .
  types:
    BEGIN OF x_w_csks,
        kokrs TYPE kokrs,
        kostl	TYPE kostl,
        datbi TYPE datbi,
        prctr TYPE prctr,
      END OF x_w_csks .
  types:
    x_t_csks TYPE SORTED TABLE OF x_w_csks WITH UNIQUE KEY primary_key COMPONENTS kokrs kostl datbi .
  types:
    BEGIN OF x_w_tka02,
        bukrs	TYPE bukrs,
        kokrs TYPE kokrs,
      END OF x_w_tka02 .
  types:
    x_t_tka02 TYPE SORTED TABLE OF x_w_tka02 WITH UNIQUE KEY primary_key COMPONENTS bukrs .
  types:
    BEGIN OF x_w_aufk,
        aufnr	TYPE aufnr,
        prctr TYPE prctr,
      END OF x_w_aufk .
  types:
    x_t_aufk TYPE SORTED TABLE OF x_w_aufk WITH UNIQUE KEY primary_key COMPONENTS aufnr .
  PROTECTED SECTION.
private section.

  class-data MT_PRPS type X_T_PRPS .
  class-data MT_CSKS type X_T_CSKS .
  class-data MT_TKA02 type X_T_TKA02 .
  class-data MT_FIELD_CFG type X_T_FCFG .
  class-data MT_AUFK type X_T_AUFK .

  methods GET_WBS_PROFIT_CENTER
    importing
      !IM_V_PSPNR type PS_POSNR
    returning
      value(RE_V_PRCTR) type PRCTR .
  methods GET_KOSTL_PROFIT_CENTER
    importing
      !IM_V_KOSTL type KOSTL
      !IM_V_KOKRS type KOKRS optional
      !IM_V_BUKRS type BUKRS
      !IM_V_BUDAT type BUDAT
    returning
      value(RE_V_PRCTR) type PRCTR .
  methods GET_ITEM_TEXT
    importing
      !IM_S_ACCIT type ACCIT
      !IM_S_DOCUMENT type CTE_S_FIN_POST_DOCUMENT
    exporting
      !EX_T_MESSAGE type CTE_T_FIN_POST_MESSAGE
    returning
      value(RE_V_SGTXT) type SGTXT .
  methods UPDATE_ITEM
    importing
      !IM_S_DOCUMENT type CTE_S_FIN_POST_DOCUMENT
    exporting
      !EX_T_MESSAGE type CTE_T_FIN_POST_MESSAGE
    changing
      !CH_S_ACCIT type ACCIT .
  methods ZZ_CHANGE_ITEMS
    importing
      !IM_S_DOCUMENT_DATA type CTE_S_FIN_POST_DOCUMENT
    exporting
      !EX_T_MESSAGE type CTE_T_FIN_POST_MESSAGE
    changing
      !CH_T_ACCCR type CTE_T_ACCCR
      !CH_T_ACCIT type CTE_T_ACCIT
      !CH_T_ACCHD type CTE_T_ACCHD
      !CH_T_ACCFI type CTE_T_ACCFI
      !CH_T_ACCTX type CTE_T_ACCBSET .
  methods GET_FIELD_CONFIG
    importing
      !IM_V_BUKRS type BUKRS
      !IM_V_ITEM_TYPE type /DS1/CTE_ITEM_TYPE
    exporting
      !EX_S_FCFG type X_W_FCFG .
  methods GET_ASSIGNMENT
    importing
      !IM_S_ACCIT type ACCIT
      !IM_S_DOCUMENT type CTE_S_FIN_POST_DOCUMENT
    exporting
      !EX_T_MESSAGE type CTE_T_FIN_POST_MESSAGE
    returning
      value(RE_V_ZUONR) type DZUONR .
  methods UPDATE_PRCTR
    importing
      !IM_S_DOCUMENT_DATA type CTE_S_FIN_POST_DOCUMENT
    exporting
      !EX_T_MESSAGE type CTE_T_FIN_POST_MESSAGE
    changing
      !CH_T_ACCIT type CTE_T_ACCIT .
  methods ZZ_CHANGE_ITEM
    importing
      !IM_S_DOCUMENT type CTE_S_FIN_POST_DOCUMENT
    exporting
      !EX_T_MESSAGE type CTE_T_FIN_POST_MESSAGE
    changing
      !CH_S_ACCIT type ACCIT .
  methods GET_DEFAULT_PROFIT_CENTER
    importing
      !IM_V_BUDAT type BUDAT
      !IM_S_DOCUMENT type CTE_S_FIN_POST_DOCUMENT
    exporting
      !EX_T_MESSAGE type CTE_T_FIN_POST_MESSAGE
    returning
      value(RE_V_PRCTR) type PRCTR .
  methods GET_AUFNR_PROFIT_CENTER
    importing
      !IM_V_AUFNR type AUFNR
    returning
      value(RE_V_AUFNR) type AUFNR .
ENDCLASS.



CLASS ZCL_FRE_CTE_FIN_ADJ_R022 IMPLEMENTATION.


  METHOD GET_ASSIGNMENT.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... /DS1/CL_FRE_CTE_FIN_ADJ_DOC
*                   ->GET_ITEM_TEXT
* TITLE......... Get FI Line Item Text
* AUTHOR........ Lenjalun Haolai
* DATE WRITTEN.. 10-Feb-2017
* R/3 RELEASE... SAP ECC6.0
* REV-TRAC...... 91232
* TRANSPORT..... D94K9A7APA
* COPIED FROM... N/A
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* This is determine FI line item text
*----------------------------------------------------------------------*
* PROGRAM TYPE.. CLASS METHOD
* PACKAGE....... /DS1/FSS_01
* LOGICAL DB.... N/A
************************************************************************

    DATA: lw_fcfg      TYPE x_w_fcfg.

    DATA: lv_item_type TYPE /ds1/cte_item_type,
          lv_pernr     TYPE pernr_d.

    "get item type
    lv_item_type = im_s_accit-sgtxt+0(3).

    "get item text config
    me->get_field_config(
      EXPORTING
        im_v_bukrs     = im_s_accit-bukrs
        im_v_item_type = lv_item_type
      IMPORTING
        ex_s_fcfg      = lw_fcfg ).

    "prepate item text
    CASE lw_fcfg-zuonr_fid.
      WHEN /ds1/if_fre_cte_constants=>c_zuonr_fid-i01.
        "Format: Date of Trip posting

        "get submit date
        TRY .
            re_v_zuonr  = im_s_document-additional_data[
              name = /ds1/if_fre_cte_constants=>c_field_name-submit_date ]-value.
          CATCH cx_sy_itab_line_not_found.
            "add error message
            /ds1/cl_fre_cte_helper=>add_message(
              EXPORTING
                im_v_report_id   = im_s_document-report_id
                im_v_revision_id = im_s_document-revision_id
                im_v_msgv1       = /ds1/if_fre_cte_constants=>c_field_name-submit_date
              IMPORTING
                ex_t_message     = ex_t_message ).
        ENDTRY.

      WHEN OTHERS.
        "default or I14

        "Format: Emp. Number
        TRY .
            lv_pernr = im_s_document-additional_data[
              name = /ds1/if_fre_cte_constants=>c_field_name-pernr ]-value.
*            lv_pernr = re_v_zuonr.
            re_v_zuonr = lv_pernr.  "Defect 340846
          CATCH cx_sy_itab_line_not_found..
            "add error message
            /ds1/cl_fre_cte_helper=>add_message(
              EXPORTING
                im_v_report_id   = im_s_document-report_id
                im_v_revision_id = im_s_document-revision_id
                im_v_msgv1       = /ds1/if_fre_cte_constants=>c_field_name-pernr
              IMPORTING
                ex_t_message     = ex_t_message ).
        ENDTRY.

    ENDCASE. "CASE lw_fcfg-sgtxt_fid.

  ENDMETHOD.


  METHOD GET_AUFNR_PROFIT_CENTER.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... /DS1/CL_FRE_CTE_FIN_ADJ_DOC
*                   ->GET_WBS_PROFIT_CENTER
* TITLE......... Get Profit Center of WBS
* AUTHOR........ Lenjalun Haolai
* DATE WRITTEN.. 10-Feb-2017
* R/3 RELEASE... SAP ECC6.0
* REV-TRAC...... 91232
* TRANSPORT..... D94K9A7APA
* COPIED FROM... N/A
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* This is fetch the Cost Center of WBS
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

    "check in buffer
    READ TABLE mt_aufk INTO DATA(lw_aufk)
      WITH TABLE KEY aufnr = im_v_aufnr.
    IF sy-subrc IS NOT INITIAL.
      "get profit center of the Order
      SELECT SINGLE aufnr prctr FROM aufk INTO lw_aufk
        WHERE aufnr = im_v_aufnr
          AND astkz = abap_false.
      IF sy-subrc IS NOT INITIAL.
        lw_aufk-aufnr = im_v_aufnr.
      ENDIF.
      INSERT lw_aufk INTO TABLE mt_aufk.
    ENDIF.

    "return value
    re_v_aufnr = lw_aufk-prctr.

  ENDMETHOD.


  METHOD GET_DEFAULT_PROFIT_CENTER.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... /DS1/CL_FRE_CTE_FIN_ADJ_DOC
*                   ->GET_KOSTL_PROFIT_CENTER
* TITLE......... Get Default Profit Center
* AUTHOR........ Lenjalun Haolai
* DATE WRITTEN.. 10-Feb-2017
* R/3 RELEASE... SAP ECC6.0
* REV-TRAC...... 91232
* TRANSPORT..... D94K9A7APA
* COPIED FROM... N/A
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* This is fetch the Profit Center of Default Employee Cost Center
*----------------------------------------------------------------------*
* PROGRAM TYPE.. CLASS METHOD
* PACKAGE....... /DS1/FSS_01
* LOGICAL DB.... N/A
************************************************************************

    "local data declaration
    DATA: lv_bukrs TYPE bukrs,
          lv_kostl TYPE kostl.

    "get employee company code
    TRY .
        lv_bukrs  = im_s_document-additional_data[
          name = /ds1/if_fre_cte_constants=>c_field_name-employee_company_code ]-value.
      CATCH cx_sy_itab_line_not_found.
        "add error message
        /ds1/cl_fre_cte_helper=>add_message(
          EXPORTING
            im_v_report_id   = im_s_document-report_id
            im_v_revision_id = im_s_document-revision_id
            im_v_msgv1       = /ds1/if_fre_cte_constants=>c_field_name-employee_company_code
          IMPORTING
            ex_t_message     = ex_t_message ).

        RETURN.
    ENDTRY.

    "get employee cost center
    TRY .
        lv_kostl  = im_s_document-additional_data[
          name = /ds1/if_fre_cte_constants=>c_field_name-employee_cost_center ]-value.
      CATCH cx_sy_itab_line_not_found.
        "add error message
        /ds1/cl_fre_cte_helper=>add_message(
          EXPORTING
            im_v_report_id   = im_s_document-report_id
            im_v_revision_id = im_s_document-revision_id
            im_v_msgv1       = /ds1/if_fre_cte_constants=>c_field_name-employee_cost_center
          IMPORTING
            ex_t_message     = ex_t_message ).

        RETURN.
    ENDTRY.

    "get profit center
    re_v_prctr = me->get_kostl_profit_center(
        im_v_kostl = lv_kostl
        im_v_bukrs = lv_bukrs
        im_v_budat = im_v_budat ).

  ENDMETHOD.


  METHOD GET_FIELD_CONFIG.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... /DS1/CL_FRE_CTE_FIN_ADJ_DOC
*                   ->GET_FIELD_CONFIG
* TITLE......... Get FI Line Item Field Format Config
* AUTHOR........ Lenjalun Haolai
* DATE WRITTEN.. 10-Feb-2017
* R/3 RELEASE... SAP ECC6.0
* REV-TRAC...... 91232
* TRANSPORT..... D94K9A7APA
* COPIED FROM... N/A
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* This is determine FI line item field format config
*----------------------------------------------------------------------*
* PROGRAM TYPE.. CLASS METHOD
* PACKAGE....... /DS1/FSS_01
* LOGICAL DB.... N/A
************************************************************************

    "get item text and assignment field config
    "first check in buffer
    READ TABLE me->mt_field_cfg INTO DATA(lw_fcfg)
      WITH TABLE KEY primary_key
      COMPONENTS bukrs = im_v_bukrs item_type = im_v_item_type.
    IF sy-subrc IS NOT INITIAL.
      "data not available in buffer

      "read config
      SELECT SINGLE bukrs item_type sgtxt_fid zuonr_fid
        INTO (lw_fcfg-bukrs, lw_fcfg-item_type, lw_fcfg-sgtxt_fid, lw_fcfg-zuonr_fid)
        FROM /ds1/cte_mt_fcfg
        WHERE bukrs = im_v_bukrs AND item_type = im_v_item_type.
      IF sy-subrc IS INITIAL.
        "config is maintained
        lw_fcfg-exists = abap_true.
      ELSE.
        "config not maintained
        lw_fcfg-bukrs = im_v_bukrs.
        lw_fcfg-item_type = im_v_item_type.

        "set to default type
        lw_fcfg-sgtxt_fid = /ds1/if_fre_cte_constants=>c_sgtxt_fid-default.
        lw_fcfg-zuonr_fid = /ds1/if_fre_cte_constants=>c_zuonr_fid-default.
      ENDIF.

      "add to buffer
      INSERT lw_fcfg INTO TABLE me->mt_field_cfg.

    ENDIF.

    "return data
    ex_s_fcfg = lw_fcfg.

  ENDMETHOD.


  METHOD GET_ITEM_TEXT.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... /DS1/CL_FRE_CTE_FIN_ADJ_DOC
*                   ->GET_ITEM_TEXT
* TITLE......... Get FI Line Item Text
* AUTHOR........ Lenjalun Haolai
* DATE WRITTEN.. 10-Feb-2017
* R/3 RELEASE... SAP ECC6.0
* REV-TRAC...... 91232
* TRANSPORT..... D94K9A7APA
* COPIED FROM... N/A
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* This is determine FI line item text
*----------------------------------------------------------------------*
* PROGRAM TYPE.. CLASS METHOD
* PACKAGE....... /DS1/FSS_01
* LOGICAL DB.... N/A
************************************************************************

    DATA: lw_fcfg                TYPE x_w_fcfg.

    DATA: lv_entry_id            TYPE cte_concur_node_id,
          lv_item_type           TYPE /ds1/cte_item_type,
          lv_pernr               TYPE p_pernr,
          lv_report_name_c       TYPE c LENGTH 32,
          lv_land1               TYPE land1,
          lv_expense_type_code_c TYPE c LENGTH 5,
          lv_last_name_c         TYPE c LENGTH 10,
          lv_submit_date         LIKE sy-datum.

    "get item type
    lv_item_type = im_s_accit-sgtxt+0(3).

    "get entry id
    lv_entry_id = im_s_accit-sgtxt+4.

    "get item text config
    me->get_field_config(
      EXPORTING
        im_v_bukrs     = im_s_accit-bukrs
        im_v_item_type = lv_item_type
      IMPORTING
        ex_s_fcfg      = lw_fcfg ).

    "get pernr
    TRY .
        lv_pernr = im_s_document-additional_data[
          name = /ds1/if_fre_cte_constants=>c_field_name-pernr ]-value.
      CATCH cx_sy_itab_line_not_found.
        "add error message
        /ds1/cl_fre_cte_helper=>add_message(
              EXPORTING
                im_v_report_id   = im_s_document-report_id
                im_v_revision_id = im_s_document-revision_id
                im_v_msgv1       = /ds1/if_fre_cte_constants=>c_field_name-pernr
              IMPORTING
                ex_t_message     = ex_t_message ).
    ENDTRY.

    "get reference to entry ID
    READ TABLE im_s_document-entry INTO DATA(lw_entry)
      WITH KEY entry_id = lv_entry_id.
    "no sy-subrc check needed

    "read allocation
    READ TABLE lw_entry-allocation INTO DATA(lw_allocation) INDEX 1.
    "no sy-subrc check needed

    "read tax
    READ TABLE lw_allocation-tax INTO DATA(lw_tax) INDEX 1.
    "no sy-subrc check needed

    "get report name in CHARACTER variable
    lv_report_name_c = im_s_document-report_name.

    "get expense type code
    lv_expense_type_code_c = lw_entry-expense_type_code.

    "get last name
    IF ( lw_fcfg-sgtxt_fid EQ /ds1/if_fre_cte_constants=>c_sgtxt_fid-f03
      OR lw_fcfg-sgtxt_fid EQ /ds1/if_fre_cte_constants=>c_sgtxt_fid-f02 )
      AND lv_pernr IS NOT INITIAL.

      TRY .
          "get submit date
          lv_submit_date = im_s_document-additional_data[
            name = /ds1/if_fre_cte_constants=>c_field_name-submit_date ]-value.

          "read last name of employee
          SELECT nachn INTO lv_last_name_c FROM pa0002 UP TO 1 ROWS
            WHERE pernr EQ lv_pernr
              AND endda GE lv_submit_date
              AND begda LE lv_submit_date.
          ENDSELECT.
        CATCH cx_sy_itab_line_not_found.
          "add error message
          /ds1/cl_fre_cte_helper=>add_message(
              EXPORTING
                im_v_report_id   = im_s_document-report_id
                im_v_revision_id = im_s_document-revision_id
                im_v_msgv1       = /ds1/if_fre_cte_constants=>c_field_name-submit_date
              IMPORTING
                ex_t_message     = ex_t_message ).
      ENDTRY.
    ENDIF. "IF ( lw_fcfg-sgtxt_fid EQ /DS1/if_fre_cte_constants=>c_sgtxt_fid-f03...

    "prepate item text
    CASE lw_fcfg-sgtxt_fid.
      WHEN /ds1/if_fre_cte_constants=>c_sgtxt_fid-f03.
        "Format: Expense type[5], Emp. Number[8], Description[26], DCR[8]  (three for spaces)
        CONCATENATE lv_expense_type_code_c /ds1/if_fre_cte_constants=>c_symbol-blank
                    lv_pernr /ds1/if_fre_cte_constants=>c_symbol-blank
                    lv_report_name_c+0(26) /ds1/if_fre_cte_constants=>c_symbol-blank
                    im_s_document-report_key INTO re_v_sgtxt RESPECTING BLANKS.

      WHEN /ds1/if_fre_cte_constants=>c_sgtxt_fid-f02.
        "Format: Emp. number[8], surname[10], short description[21] & Trip number[8]   (three for spaces)
        CONCATENATE lv_pernr /ds1/if_fre_cte_constants=>c_symbol-blank
                    lv_last_name_c /ds1/if_fre_cte_constants=>c_symbol-blank
                    lv_report_name_c+0(21) /ds1/if_fre_cte_constants=>c_symbol-blank
                    im_s_document-report_key INTO re_v_sgtxt RESPECTING BLANKS.

      WHEN /ds1/if_fre_cte_constants=>c_sgtxt_fid-f08.
        "Format: Emp. no.[8], surname[10], short description[18], Trip number[8] & tax code[2]   (four for spaces)
        CONCATENATE lv_pernr /ds1/if_fre_cte_constants=>c_symbol-blank
                    lv_last_name_c /ds1/if_fre_cte_constants=>c_symbol-blank
                    lv_report_name_c+0(18) /ds1/if_fre_cte_constants=>c_symbol-blank
                    im_s_document-report_key /ds1/if_fre_cte_constants=>c_symbol-blank
                    lw_tax-tax_code INTO re_v_sgtxt RESPECTING BLANKS.

      WHEN OTHERS.
        "default or FO5
        "Description[24], Country Key[2], Expense type[4], Space[1], Emp. Number[8], T, Trip number[8]
        "This format is as per OTC requirement.
        "NOTE: - Report name i.e. description and expense type code will be truncated
        "      - Last Two position will be left blank

        "get home country code
        TRY .
            lv_land1 = im_s_document-additional_data[
              name = /ds1/if_fre_cte_constants=>c_field_name-home_country_code ]-value.
          CATCH cx_sy_itab_line_not_found.
            "get from employee country code
            lv_land1 = cl_cte_fin_post_tax_helper=>get_employee_country(
              iv_company_code_employee = im_s_document-company_code_employee ).
        ENDTRY.

        "prepare text
        CONCATENATE lv_report_name_c+0(24) lv_land1+0(2) lv_expense_type_code_c+0(4)
                    /ds1/if_fre_cte_constants=>c_symbol-blank lv_pernr
                    /ds1/if_fre_cte_constants=>c_char-t im_s_document-report_key
                    INTO re_v_sgtxt RESPECTING BLANKS.

    ENDCASE. "CASE lw_fcfg-sgtxt_fid.

  ENDMETHOD.


  METHOD GET_KOSTL_PROFIT_CENTER.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... /DS1/CL_FRE_CTE_FIN_ADJ_DOC
*                   ->GET_KOSTL_PROFIT_CENTER
* TITLE......... Get Profit Center of Cost Center
* AUTHOR........ Lenjalun Haolai
* DATE WRITTEN.. 10-Feb-2017
* R/3 RELEASE... SAP ECC6.0
* REV-TRAC...... 91232
* TRANSPORT..... D94K9A7APA
* COPIED FROM... N/A
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* This is fetch the Profit Center of Cost Center
*----------------------------------------------------------------------*
* PROGRAM TYPE.. CLASS METHOD
* PACKAGE....... /DS1/FSS_01
* LOGICAL DB.... N/A
************************************************************************

    DATA: lv_kokrs      TYPE kokrs.

    "check if controlling area is provided
    IF im_v_kokrs IS INITIAL.
      "check if buffer
      READ TABLE mt_tka02 INTO DATA(lw_tka02)
        WITH TABLE KEY primary_key COMPONENTS
        bukrs = im_v_bukrs.
      IF sy-subrc IS NOT INITIAL.
        "fetch controlling area
        " 1:N relationship exists between controlling area and company code
        SELECT bukrs kokrs FROM tka02 INTO lw_tka02 UP TO 1 ROWS
          WHERE bukrs = im_v_bukrs.
        ENDSELECT.
        IF sy-subrc IS NOT INITIAL.
          lw_tka02-bukrs = im_v_bukrs.
        ENDIF.
        "add to buffer
        INSERT lw_tka02 INTO TABLE mt_tka02.
      ENDIF.
      "get controlling area
      lv_kokrs = lw_tka02-kokrs.
    ELSE.
      lv_kokrs = im_v_kokrs.
    ENDIF.

    "check in buffer
    LOOP AT mt_csks INTO DATA(lw_csks) USING KEY primary_key
      WHERE kokrs = lv_kokrs
        AND kostl = im_v_kostl
        AND datbi GE im_v_budat.
      EXIT.
    ENDLOOP.
    IF sy-subrc IS NOT INITIAL.
      "get profit center of the Cost Center
      SELECT SINGLE kokrs kostl datbi prctr FROM csks INTO lw_csks
        WHERE kokrs = lv_kokrs
          AND kostl = im_v_kostl
          AND datbi GE im_v_budat.
      IF sy-subrc IS NOT INITIAL.
        lw_csks-kokrs = lv_kokrs.
        lw_csks-kostl = im_v_kostl.
        lw_csks-datbi = im_v_budat.
      ENDIF.
      "add to buffer
      INSERT lw_csks INTO TABLE mt_csks.
    ENDIF.

    "return value
    re_v_prctr = lw_csks-prctr.

  ENDMETHOD.


  METHOD GET_WBS_PROFIT_CENTER.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... /DS1/CL_FRE_CTE_FIN_ADJ_DOC
*                   ->GET_WBS_PROFIT_CENTER
* TITLE......... Get Profit Center of WBS
* AUTHOR........ Lenjalun Haolai
* DATE WRITTEN.. 10-Feb-2017
* R/3 RELEASE... SAP ECC6.0
* REV-TRAC...... 91232
* TRANSPORT..... D94K9A7APA
* COPIED FROM... N/A
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* This is fetch the Profit Center of WBS
*----------------------------------------------------------------------*
* PROGRAM TYPE.. CLASS METHOD
* PACKAGE....... /DS1/FSS_01
* LOGICAL DB.... N/A
************************************************************************
    "check in buffer
    READ TABLE mt_prps INTO DATA(lw_prps)
      WITH TABLE KEY pspnr = im_v_pspnr.
    IF sy-subrc IS NOT INITIAL.
      "get profit center of the WBS
      SELECT SINGLE pspnr prctr FROM prps INTO lw_prps
        WHERE pspnr = im_v_pspnr
          AND xstat = abap_false.
      IF sy-subrc IS NOT INITIAL.
        lw_prps-pspnr = im_v_pspnr.
      ENDIF.
      INSERT lw_prps INTO TABLE mt_prps.
    ENDIF.

    "return value
    re_v_prctr = lw_prps-prctr.

  ENDMETHOD.


  METHOD IF_BADI_CTE_FIN_POST_ADJ_DOC~ADJUST_AD_POSTING_DOCUMENT.

  ENDMETHOD.


  METHOD IF_BADI_CTE_FIN_POST_ADJ_DOC~ADJUST_POSTING_DOCUMENT.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... /DS1/CL_FRE_CTE_FIN_ADJ_DOC
*                   ->IF_BADI_CTE_FIN_POST_ADJ_DOC~ADJUST_POSTING_DOCUMENT
* TITLE......... Update FI Document before posting
* AUTHOR........ Lenjalun Haolai
* DATE WRITTEN.. 10-Feb-2017
* R/3 RELEASE... SAP ECC6.0
* REV-TRAC...... 91232
* TRANSPORT..... D94K9A7APA
* COPIED FROM... N/A
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* This is update FI document before posting
*----------------------------------------------------------------------*
* PROGRAM TYPE.. CLASS METHOD
* PACKAGE....... /DS1/FSS_01
* LOGICAL DB.... N/A
************************************************************************

    "local data declaration
    DATA: lt_message    LIKE et_message.

    DATA: lv_pernr     TYPE pernr_d,
          lv_item_type TYPE /ds1/cte_item_type.

    "update profit center
    me->update_prctr(
      EXPORTING
        im_s_document_data = is_document_data
      IMPORTING
        ex_t_message       = et_message
      CHANGING
        ch_t_accit         = ct_accit ).

    "custom changes to line items (ERP specific)
    me->zz_change_items(
      EXPORTING
        im_s_document_data = is_document_data
      IMPORTING
        ex_t_message       = lt_message
      CHANGING
        ch_t_acccr         = ct_acccr
        ch_t_accit         = ct_accit
        ch_t_acchd         = ct_acchd
        ch_t_accfi         = ct_accfi
        ch_t_acctx         = ct_acctx ).


    "perform final updates
    LOOP AT ct_accit ASSIGNING FIELD-SYMBOL(<lw_accit>).
      "get item type
      lv_item_type = <lw_accit>-sgtxt+0(3).

      "changes to expense line items
      IF lv_item_type EQ cl_cte_fin_post_er_use_case=>mc_item_type_exp_account
        OR lv_item_type EQ cl_cte_fin_post_er_use_case=>mc_item_type_exp_act_credit.
        "for OOP_REMOVE scenario, remove the cost objects and tax codes
        IF <lw_accit>-sgtxt+37(10) EQ /ds1/if_fre_cte_constants=>c_use_case_id-oop_remove.
          CLEAR: <lw_accit>-kostl, <lw_accit>-ps_psp_pnr,
                 <lw_accit>-mwskz, <lw_accit>-txjcd.
        ENDIF.

        "set PERNR
*        <lw_accit>-pernr = lv_pernr. "Commented after comparing with SERP System
      ENDIF.

      "changes to line item
      me->update_item(
          EXPORTING
            im_s_document = is_document_data
          IMPORTING
            ex_t_message  = lt_message
          CHANGING
            ch_s_accit    = <lw_accit> ).

      "changes to line item(ERP Specific)
      me->zz_change_item(
          EXPORTING
            im_s_document = is_document_data
          IMPORTING
            ex_t_message  = lt_message
          CHANGING
            ch_s_accit    = <lw_accit> ).

      "change item text
      " - this should be the last item to update as it contains the link
      "   between control and expense line items. Once updated, this link
      "   will be lost.
      <lw_accit>-sgtxt = me->get_item_text(
                          EXPORTING
                            im_s_accit      = <lw_accit>
                            im_s_document   = is_document_data
                          IMPORTING
                            ex_t_message    = lt_message ).

    ENDLOOP. "LOOP AT ct_accit ASSIGNING FIELD-SYMBOL(<lw_accit>).
  ENDMETHOD.


  METHOD UPDATE_ITEM.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... /DS1/CL_FRE_CTE_FIN_ADJ_DOC
*                   ->CHANGE_ITEM
* TITLE......... Update FI Line Item
* AUTHOR........ Lenjalun Haolai
* DATE WRITTEN.. 10-Feb-2017
* R/3 RELEASE... SAP ECC6.0
* REV-TRAC...... 91232
* TRANSPORT..... D94K9A7APA
* COPIED FROM... N/A
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* This is update FI Line Item data
*----------------------------------------------------------------------*
* PROGRAM TYPE.. CLASS METHOD
* PACKAGE....... /DS1/FSS_01
* LOGICAL DB.... N/A
************************************************************************

    "update header text
*    ch_s_accit-xref1_hd = im_s_document-report_id.   " Commented after comparing with SERP system
    DATA: lv_entry_id TYPE cte_concur_node_id.

    "change assignment
    ch_s_accit-zuonr    = me->get_assignment(
      EXPORTING
        im_s_accit      = ch_s_accit
        im_s_document   = im_s_document
      IMPORTING
        ex_t_message    = ex_t_message ).

    "there are scenarios where tax code is not applicable
    " hence, look back at config and clear tax code as per config
    IF ch_s_accit-mwskz IS NOT INITIAL.
      " get original tax code
      "get entry id
      lv_entry_id = ch_s_accit-sgtxt+4.

      IF lv_entry_id IS NOT INITIAL.    "to check
        "get CTE tax code (do not use any exception handling; it should error if original tax code is missing)
        IF im_s_document-additional_data[ name = /ds1/if_fre_cte_constants=>c_field_name-home_country_code ]-value
          EQ im_s_document-entry[ entry_id = lv_entry_id ]-allocation[ 1 ]-tax[ 1 ]-tax_country_code.
          "domestic taxes
          ch_s_accit-mwskz = im_s_document-entry[ entry_id = lv_entry_id ]-allocation[ 1 ]-additional_data[
            name = /ds1/if_fre_cte_constants=>c_field_name-cte_dom_tax_code ]-value.
        ELSE.
          "non-domestic taxes
          ch_s_accit-mwskz = im_s_document-entry[ entry_id = lv_entry_id ]-allocation[ 1 ]-additional_data[
            name = /ds1/if_fre_cte_constants=>c_field_name-cte_nond_tax_code ]-value.
        ENDIF.
        " get original tax code

        " Update tax code
        /ds1/cl_fre_cte_helper=>get_taxcode(
          EXPORTING
            im_v_bukrs      = ch_s_accit-bukrs
            im_v_check_init = abap_true
          CHANGING
            ch_v_taxcode    = ch_s_accit-mwskz ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD UPDATE_PRCTR.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... /DS1/CL_FRE_CTE_FIN_ADJ_DOC
*                   ->UPDATE_PRCTR
* TITLE......... Update FI Line Item
* AUTHOR........ Lenjalun Haolai
* DATE WRITTEN.. 10-Feb-2017
* R/3 RELEASE... SAP ECC6.0
* REV-TRAC...... 91232
* TRANSPORT..... D94K9A7APA
* COPIED FROM... N/A
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* This is update FI Line Item data
*----------------------------------------------------------------------*
* PROGRAM TYPE.. CLASS METHOD
* PACKAGE....... /DS1/FSS_01
* LOGICAL DB.... N/A
************************************************************************
************************************************************************
* CHANGE HISTORY                                                       *
************************************************************************
* DATE CHANGE... 19-July-2017
* AUTHOR........ Lenjalun Haolai (INPCK1)
* CHANGE DESCR.. For Control lines, get Profit Center of Cost Objects
*                 available in costrol lines itself, then clear the
*                 cost objects
* R/3 RELEASE... SAP ECC6.0
* REV TRAC NO... 91232 / D94K9A7GA9
************************************************************************
    "local data declaration
*    TYPES:BEGIN OF ty_accit,
*            entry_id   TYPE cte_concur_node_id,
*            kokrs      TYPE kokrs,
*            kostl      TYPE kostl,
*            bukrs      TYPE bukrs,
*            budat      TYPE budat,
*            gsber      TYPE gsber,
*            ps_psp_pnr TYPE ps_psp_pnr,
*          END OF ty_accit.

    "DATA: lt_accit     TYPE STANDARD TABLE OF ty_accit.        "--GCR30391736/RT16499

    DATA: lv_item_type TYPE /ds1/cte_item_type.

    "start of GCR30391736/RT16499: local copy of expense lines are not required
*    "create a copy of the line items fields required for processing
*    LOOP AT ch_t_accit ASSIGNING FIELD-SYMBOL(<lw_accit>).
*      "get item type
*      lv_item_type = <lw_accit>-sgtxt+0(3).
*
*      "only expense line items
*      CHECK lv_item_type EQ cl_cte_fin_post_er_use_case=>mc_item_type_exp_account
*        OR lv_item_type EQ cl_cte_fin_post_er_use_case=>mc_item_type_exp_act_credit.
*
*      "collect expense line items
*      APPEND INITIAL LINE TO lt_accit ASSIGNING FIELD-SYMBOL(<lw_accit_exp>).
*      <lw_accit_exp>-entry_id = <lw_accit>-sgtxt+4(32).
*      <lw_accit_exp>-ps_psp_pnr = <lw_accit>-ps_psp_pnr.
*      <lw_accit_exp>-bukrs = <lw_accit>-bukrs.
*      <lw_accit_exp>-gsber = <lw_accit>-gsber.
*      <lw_accit_exp>-kokrs = <lw_accit>-kokrs.
*      <lw_accit_exp>-kostl = <lw_accit>-kostl.
*      <lw_accit_exp>-budat = <lw_accit>-budat.
*    ENDLOOP. "LOOP AT ch_t_accit ASSIGNING FIELD-SYMBOL(<lw_accit>).
*
*    "sort expense line items
*    SORT lt_accit BY entry_id.
    "end of GCR30391736/RT16499: local copy of expense lines are not required

    "update profit center line items
    "LOOP AT ch_t_accit ASSIGNING <lw_accit>.                         "--GCR30391736/RT16499
    LOOP AT ch_t_accit ASSIGNING FIELD-SYMBOL(<lw_accit>).            "++GCR30391736/RT16499
      "get item type
      lv_item_type = <lw_accit>-sgtxt+0(3).

      "in disallowed scenario, profit center to be derived from
      " employee default cost center
      IF <lw_accit>-sgtxt+37(10) EQ /ds1/if_fre_cte_constants=>c_use_case_id-disallowed.
        "get profit center of employee default cost center
        <lw_accit>-prctr = me->get_default_profit_center(
          EXPORTING
            im_s_document = im_s_document_data
            im_v_budat = <lw_accit>-budat
          IMPORTING
            ex_t_message = ex_t_message  ).

        "continue with next item
        CONTINUE.
      ENDIF. "IF <lw_accit>-sgtxt+37(10) EQ /ds1/if_fre_cte_constants=>c_use_case_id-disallowed.

      "in oop_remove scenario, get profit center for expense line item
      " as the cost objects will be cleared
      IF lv_item_type EQ cl_cte_fin_post_er_use_case=>mc_item_type_exp_account
        OR lv_item_type EQ cl_cte_fin_post_er_use_case=>mc_item_type_exp_act_credit.
        IF <lw_accit>-sgtxt+37(10) EQ /ds1/if_fre_cte_constants=>c_use_case_id-oop_remove.
          "get profit center from the cost object (WBS or Cost Center)
          IF <lw_accit>-ps_psp_pnr IS NOT INITIAL.
            "get profit center of the WBS
            <lw_accit>-prctr = me->get_wbs_profit_center(
              im_v_pspnr = <lw_accit>-ps_psp_pnr ).
          ELSEIF <lw_accit>-kostl IS NOT INITIAL.
            "get profit center of the Cost Center
            <lw_accit>-prctr = me->get_kostl_profit_center(
                im_v_kostl = <lw_accit>-kostl
                im_v_kokrs = <lw_accit>-kokrs
                im_v_bukrs = <lw_accit>-bukrs
                im_v_budat = <lw_accit>-budat ).
          ELSEIF <lw_accit>-aufnr IS NOT INITIAL.
            "get profit center of the WBS
            <lw_accit>-prctr = me->get_aufnr_profit_center(
              im_v_aufnr = <lw_accit>-aufnr ).
          ENDIF. "IF <lw_accit_exp>-ps_psp_pnr IS NOT INITIAL.
        ENDIF. "IF <lw_accit>-sgtxt+37(10) EQ /ds1/if_fre_cte_constants=>c_use_case_id-oop_remove.

      ELSEIF lv_item_type EQ cl_cte_fin_post_er_use_case=>mc_item_type_cc_clearing
        OR lv_item_type EQ cl_cte_fin_post_er_use_case=>mc_item_type_cc_clear_db.
        "determine profit center for control line item

        "start of GCR30391736/RT16499: get profit center from associated cost object
*        "get corresponding expense line item
*        READ TABLE lt_accit ASSIGNING <lw_accit_exp>
*          WITH KEY entry_id = <lw_accit>-sgtxt+4(32) BINARY SEARCH.
*        IF sy-subrc IS INITIAL.
*          "get profit center from the cost object (WBS or Cost Center)
*          IF <lw_accit_exp>-ps_psp_pnr IS NOT INITIAL.
*            "get profit center of the WBS
*            <lw_accit>-prctr = me->get_wbs_profit_center(
*              im_v_pspnr = <lw_accit_exp>-ps_psp_pnr ).
*          ELSEIF <lw_accit_exp>-kostl IS NOT INITIAL.
*            "get profit center of the Cost Center
*            <lw_accit>-prctr = me->get_kostl_profit_center(
*                im_v_kostl = <lw_accit_exp>-kostl
*                im_v_kokrs = <lw_accit_exp>-kokrs
*                im_v_bukrs = <lw_accit_exp>-bukrs
*                im_v_budat = <lw_accit_exp>-budat ).
*          ENDIF. "IF <lw_accit_exp>-ps_psp_pnr IS NOT INITIAL.
*        ENDIF. "IF sy-subrc IS INITIAL.

        IF <lw_accit>-ps_psp_pnr IS NOT INITIAL.
          "get profit center of the WBS
          <lw_accit>-prctr = me->get_wbs_profit_center(
            im_v_pspnr = <lw_accit>-ps_psp_pnr ).
        ELSEIF <lw_accit>-kostl IS NOT INITIAL.
          "get profit center of the Cost Center
          <lw_accit>-prctr = me->get_kostl_profit_center(
              im_v_kostl = <lw_accit>-kostl
              im_v_kokrs = <lw_accit>-kokrs
              im_v_bukrs = <lw_accit>-bukrs
              im_v_budat = <lw_accit>-budat ).
          ELSEIF <lw_accit>-aufnr IS NOT INITIAL.
            "get profit center of the WBS
            <lw_accit>-prctr = me->get_aufnr_profit_center(
              im_v_aufnr = <lw_accit>-aufnr ).
        ENDIF. "IF <lw_accit>-ps_psp_pnr IS NOT INITIAL.

        "clear all cost objects
        CLEAR: <lw_accit>-ps_psp_pnr, <lw_accit>-kostl, <lw_accit>-aufnr.
        "end of GCR30391736/RT16499: get profit center from associated cost object
      ENDIF. "IF <lw_accit>-sgtxt+0(3) NE cl_cte_fin_post_er_use_case=>mc_item_type_cc_clearing.
    ENDLOOP. "LOOP AT ch_t_accit ASSIGNING <lw_accit>.

  ENDMETHOD.


  METHOD ZZ_CHANGE_ITEM.
************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... /DS1/CL_FRE_CTE_FIN_ADJ_DOC->ZZ_CHANGE_ITEM
* TITLE......... Update FI Document before posting
* AUTHOR........ Lenjalun Haolai
* DATE WRITTEN.. 10-Feb-2017
* R/3 RELEASE... SAP ECC6.0
* REV-TRAC...... 91232
* TRANSPORT..... D94K9A7APA
* COPIED FROM... N/A
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* This is update FI document line item details before posting
*----------------------------------------------------------------------*
* PROGRAM TYPE.. CLASS METHOD
* PACKAGE....... /DS1/FSS_01
* LOGICAL DB.... N/A
************************************************************************

* Commented as part zz method are to be implemented as part of SERP
    "store entry id in xref1 and xref3
*    ch_s_accit-xref1 = ch_s_accit-sgtxt+4(12). "first 12 characters
*    ch_s_accit-xref3 = ch_s_accit-sgtxt+16(20). "last 20 characters

  ENDMETHOD.


  METHOD ZZ_CHANGE_ITEMS.

  ENDMETHOD.
ENDCLASS.
