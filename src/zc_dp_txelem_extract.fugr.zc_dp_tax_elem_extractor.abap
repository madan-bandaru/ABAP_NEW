  FUNCTION ZC_DP_TAX_ELEM_EXTRACTOR .
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_REQUNR) TYPE  SRSC_S_IF_SIMPLE-REQUNR
*"     VALUE(I_DSOURCE) TYPE  SRSC_S_IF_SIMPLE-DSOURCE OPTIONAL
*"     VALUE(I_MAXSIZE) TYPE  SRSC_S_IF_SIMPLE-MAXSIZE OPTIONAL
*"     VALUE(I_INITFLAG) TYPE  SRSC_S_IF_SIMPLE-INITFLAG OPTIONAL
*"     VALUE(I_READ_ONLY) TYPE  SRSC_S_IF_SIMPLE-READONLY OPTIONAL
*"  TABLES
*"      I_T_SELECT TYPE  SRSC_S_IF_SIMPLE-T_SELECT OPTIONAL
*"      I_T_FIELDS TYPE  SRSC_S_IF_SIMPLE-T_FIELDS OPTIONAL
*"      E_T_DATA STRUCTURE  /DS1/SETI_TAX_ELEM_EXT_ST OPTIONAL
*"  EXCEPTIONS
*"      NO_MORE_DATA
*"      ERROR_PASSED_TO_MESS_HANDLER
*"--------------------------------------------------------------------
*-
***********************************************************************
*	FUNCTION INFORMATION
*----------------------------------------------------------------------*
*	FUNCTION...... /DS1/C_DP_TAX_ELEM_EXTRACTOR                          *
*	TITLE......... SETI Tax Element data extractor                       *
* OWNER......... Harish Raheja                                         *
* AUTHOR........ Aron MacDonald (GBAMHB)                               *
*	DATE WRITTEN.. 7th June 2013                                         *
*	R/3 RELEASE... R3.1                                                  *
*	TRANSPORTNR...                                                       *
*	COPIED FROM... <Cloned function>                                     *
*----------------------------------------------------------------------*
*	FUNCTION DESCRIPTION:                                                *
*	This function module will be called by a generic BW Datasource       *
*      ZDSCA_CD_DATA. This FM will extract tax data fom SETI Tax tables*
*      and other acoounting docuemnt tables.  This FM is delta enabled *
*      based on Creation date and document status of the accounting    *
*      document and has functionality of initialization call up and    *
*      subsequent calls.                                               *
************************************************************************
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

  DATA:
        lw_bukrs      LIKE LINE OF r_bukrs,
        lw_cpudt      LIKE LINE OF r_cpudt,
        lw_belnr      LIKE LINE OF r_belnr,
        lw_gjahr      LIKE LINE OF r_gjahr,
        lw_monat      LIKE LINE OF r_monat,
        l_lines       TYPE srsc_s_if_simple-maxsize,
        l_index       TYPE sy-tabix,
*Start of Insertion MOD-001
        l_hint1       TYPE /ds1/low,  " local variable to store hint
        l_hint2       TYPE /ds1/low,  " local variable to store hint
        l_hint3       TYPE /ds1/low,  " local variable to store hint
*End of Insertion MOD-001
        l_timestr     TYPE string.
*Start of Insertion MOD-001
  CONSTANTS:
        c_bussid TYPE /ds1/buss_ind1 VALUE 'SCR3473', " Business ind.
        c_repid TYPE syrepid VALUE '/DS1/C_DP_TAX_ELEM_EXTRACTOR',
                                                            " prog name
        c_hint1 TYPE fieldname VALUE 'LD_HINT_1',
                                           " entry create in Fval table
        c_hint2 TYPE fieldname VALUE 'LD_HINT_2',
                                           " entry create in Fval table
        c_hint3 TYPE fieldname VALUE 'LD_HINT_3'.
                                          " entry create in Fval table
TYPES: BEGIN OF x_hints,
       variable_name TYPE fieldname, " Field Name
       value_low TYPE /ds1/low,      " Range Low value
       END OF x_hints.
DATA : t_hints TYPE STANDARD TABLE OF x_hints, " table to store entries
                                               " created in Fval table
       w_hints TYPE x_hints.
*End of Insertion MOD-001


    IF i_initflag = sbiwa_c_flag_on.

      PERFORM refresh_global_variables.

*Check DataSource validity, request ID
      IF i_dsource IS INITIAL OR
*         i_requnr IS INITIAL  OR
         i_dsource <>  c_datasource.

        MESSAGE e009(r3) WITH i_dsource.
        log_write 'E'             "message type
                  'R3'            "message class
                  '009'           "message number
            i_dsource             "message variable 1
             ' '.                 "message variable 2
        RAISE error_passed_to_mess_handler.
      ENDIF.


      APPEND LINES OF i_t_select TO g_s_if-t_select.
      APPEND LINES OF i_t_fields TO g_s_if-t_fields.

      g_s_if-requnr    = i_requnr.
      g_s_if-dsource   = i_dsource.
      g_s_if-maxsize   = i_maxsize.
      g_s_if-initflag  = i_initflag.

      RETURN.
    ENDIF.

    IF g_initial_call = ' '.

      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname        = 'BKPF'
        TABLES
          dfies_tab      = t_dfies
        EXCEPTIONS
          not_found      = 1
          internal_error = 2
          OTHERS         = 3.
      CHECK sy-subrc = 0.

      SORT t_dfies BY fieldname.
      LOOP AT g_s_if-t_select INTO w_select.

        CASE w_select-fieldnm.
          WHEN 'CPUDT'.
*            lw_stamp-sign   = w_select-sign .
*            lw_stamp-option = 'BT'. "w_select-option.
*            l_timestr    = w_select-low.
*            concatenate l_timestr   '000000' into l_timestr.
*            lw_stamp-low = l_timestr.
*            IF w_select-high = ''.
*              l_timestr   = w_select-low.
*              concatenate l_timestr   '240000' into l_timestr.
*            ELSE.
*              l_timestr   = w_select-high.
*              concatenate l_timestr   '240000' into l_timestr.
*            ENDIF.
*            lw_stamp-high = l_timestr.
*            APPEND lw_stamp TO r_stamp.
            lw_cpudt-sign   = w_select-sign .
            lw_cpudt-option = w_select-option.
            lw_cpudt-low    = w_select-low.
            lw_cpudt-high   = w_select-high.
            APPEND lw_cpudt TO r_cpudt.


          WHEN 'BUKRS'.
            lw_bukrs-sign   = w_select-sign .
            lw_bukrs-option = w_select-option .
            lw_bukrs-low    = w_select-low .
            lw_bukrs-high   = w_select-high .
            APPEND lw_bukrs TO r_bukrs.

          WHEN 'BELNR'.
            lw_belnr-sign   = w_select-sign .
            lw_belnr-option = w_select-option .

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = w_select-low
              IMPORTING
                output = lw_belnr-low.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = w_select-high
              IMPORTING
                output = lw_belnr-high.

            APPEND lw_belnr TO r_belnr.

          WHEN 'GJAHR'.
            lw_gjahr-sign   = w_select-sign .
            lw_gjahr-option = w_select-option .
            lw_gjahr-low    = w_select-low .
            lw_gjahr-high   = w_select-high .

            APPEND lw_gjahr TO r_gjahr.

          WHEN 'MONAT'.
            lw_monat-sign   = w_select-sign .
            lw_monat-option = w_select-option .
            lw_monat-low    = w_select-low .
            lw_monat-high   = w_select-high .

            APPEND lw_monat TO r_monat.

          WHEN OTHERS.
            READ TABLE t_dfies INTO w_dfies
                  WITH KEY fieldname = w_select-fieldnm
                           BINARY SEARCH.
            IF sy-subrc = 0.
*              PERFORM dynamic_where_class.
            ENDIF.
        ENDCASE.
      ENDLOOP.


      IF r_bukrs[] IS INITIAL.
        SELECT bukrs
          FROM /txintf/ccode_te
          INTO TABLE t_ccode.
        IF sy-subrc = 0.
          LOOP AT t_ccode INTO w_ccode.
            CLEAR: lw_bukrs.
            lw_bukrs-sign = 'I'.
            lw_bukrs-option = 'EQ'.
            lw_bukrs-low = w_ccode-bukrs.
            APPEND lw_bukrs TO r_bukrs.
          ENDLOOP.
        ENDIF.
      ENDIF.

      w_where = 'COMPANY_CODE in r_bukrs'(012).
      INSERT w_where INTO t_where INDEX 1.


      IF r_belnr[] IS NOT INITIAL.
        w_where = 'and FI_DOCUMENT_NO in r_belnr'(015).
        APPEND w_where TO t_where.
      ENDIF.

      IF r_gjahr[] IS NOT INITIAL.
        w_where = 'and FI_DOCUMENT_YEAR in r_gjahr'(015).
        APPEND w_where TO t_where.
      ENDIF.


      IF r_cpudt[] IS NOT INITIAL.
*        w_where = 'and EXTRACT_LAST_CHANGE in r_stamp'.
        w_where = 'and FI_DOCUMENT_ENTRY_DATE in r_cpudt'.
        APPEND w_where TO t_where.
      ENDIF.

      IF r_monat[] IS NOT INITIAL.
*        w_where = 'and EXTRACT_LAST_CHANGE in r_stamp'.
        w_where = 'and MONAT in r_monat'.
        APPEND w_where TO t_where.
      ENDIF.

*Start of Insertion MOD-001
SELECT variable_name value_low
  FROM /ds1/bc_mt_fval
  INTO TABLE t_hints
  WHERE program_name = c_repid
  AND   bussiness_ind = c_bussid.
IF sy-subrc EQ 0.
  READ TABLE t_hints INTO w_hints WITH KEY variable_name = c_hint1.
    IF sy-subrc = 0.
      l_hint1 = w_hints-value_low.
    ENDIF.
  READ TABLE t_hints INTO w_hints WITH KEY variable_name = c_hint2.
    IF sy-subrc = 0.
      l_hint2 = w_hints-value_low.
    ENDIF.
  READ TABLE t_hints INTO w_hints WITH KEY variable_name = c_hint3.
    IF sy-subrc = 0.
      l_hint3 = w_hints-value_low.
    ENDIF.
ENDIF.
*End of Insertion MOD-001

      OPEN CURSOR WITH HOLD g_dpext FOR SELECT
           COMPANY_CODE
           FI_DOCUMENT_NO
           FI_DOCUMENT_YEAR
           FI_DOCUMENT_LINE_NO
           LIV_DOCUMENT_LINE_NO
           WO_SETTLE_SEQ
           VERSION
****Added by INSSHM
           GR_REF_DOC
           GR_REF_DOC_YEAR
           GR_REF_DOC_ITEM
****End by INSSHM
           EXTRACT_LAST_CHANGE
*Start of Insertion MOD-001
           B~CPUDT
           B~CPUTM
           B~XBLNR
           B~AWTYP
           B~AWKEY
           B~MONAT
**End of Insertion MOD-001
*       FROM  /DS1/FI_MT_DPEXT
       FROM ( /DS1/FI_MT_DPEXT AS EXT
              INNER JOIN BKPF AS B
              ON B~bukrs = EXT~COMPANY_CODE
      AND  B~belnr = EXT~FI_DOCUMENT_NO
      AND  B~gjahr = EXT~FI_DOCUMENT_YEAR )
       WHERE (t_where)
      %_HINTS ORACLE l_hint1 ORACLE l_hint2 ORACLE c_hint3. "Ins MOD-001
*      WHERE FI_DOCUMENT_NO  = '1500000052'.

      g_initial_call = 'X'.

    ENDIF.

    g_packagesize = i_maxsize.

*Start of Deletion MOD-001
*    CLEAR: w_bkpf, w_doc_ext, w_doc_out.
*
*    REFRESH: t_bkpf[], t_doc_ext[], t_doc_out[].
*End of Deletion MOD-001
*Start of Insertion MOD-001
    CLEAR: w_doc_ext, w_doc_out.

    REFRESH: t_doc_ext[], t_doc_out[].
**End of Insertion MOD-001

    IF t_data[] IS NOT INITIAL.
      DESCRIBE TABLE t_data LINES l_lines.
      IF l_lines GE g_packagesize.
        APPEND LINES OF t_data FROM 1 TO g_packagesize TO e_t_data.
        DELETE t_data FROM 1 TO g_packagesize.
        RETURN.
      ELSE.
        APPEND LINES OF t_data FROM 1 TO l_lines TO e_t_data.
        REFRESH: t_data[].
        g_packagesize = g_packagesize - l_lines.
      ENDIF.
    ENDIF.

    FETCH NEXT CURSOR g_dpext INTO TABLE t_doc_ext PACKAGE SIZE g_packagesize.
    IF sy-subrc = 0.

      IF t_doc_ext[]  IS NOT INITIAL.

        PERFORM fetch_required_data.

*       Sort tables for subsequent binary search
        SORT t_doc_ext BY FI_DOCUMENT_YEAR COMPANY_CODE FI_DOCUMENT_NO FI_DOCUMENT_LINE_NO LIV_DOCUMENT_LINE_NO WO_SETTLE_SEQ VERSION.
*        SORT t_bkpf    BY gjahr bukrs belnr . "Del MOD-001

        LOOP AT t_doc_out INTO w_doc_out.
          CLEAR: w_data.

          READ TABLE t_doc_ext INTO w_doc_ext
             WITH KEY FI_DOCUMENT_YEAR     = w_doc_out-FI_DOCUMENT_YEAR
                      COMPANY_CODE         = w_doc_out-COMPANY_CODE
                      FI_DOCUMENT_NO       = w_doc_out-FI_DOCUMENT_NO
                      FI_DOCUMENT_LINE_NO  = w_doc_out-FI_DOCUMENT_LINE_NO
                      LIV_DOCUMENT_LINE_NO = w_doc_out-LIV_DOCUMENT_LINE_NO
                      WO_SETTLE_SEQ        = w_doc_out-WO_SETTLE_SEQ
                      VERSION              = w_doc_out-VERSION
             BINARY SEARCH.

          CHECK sy-subrc = 0.

*Start of Deletion MOD-001
*          READ TABLE t_bkpf INTO w_bkpf WITH KEY gjahr = w_doc_out-FI_DOCUMENT_YEAR
*                                                 bukrs = w_doc_out-COMPANY_CODE
*                                                 belnr = w_doc_out-FI_DOCUMENT_NO
*                                                 BINARY SEARCH.
*          CHECK sy-subrc  = 0.
*
*          MOVE-CORRESPONDING w_bkpf TO w_data.
*End of Deletion MOD-001
          MOVE-CORRESPONDING w_doc_ext  TO w_data.
          w_data-TIMESTAMP = w_doc_ext-EXTRACT_LAST_CHANGE.
          MOVE-CORRESPONDING w_doc_out  TO w_data.
          w_data-CURRENCY_AU = w_doc_out-AUTHORITY_CURRENCY_CODE.

          TRANSLATE w_data-taxable_state TO UPPER CASE.
          TRANSLATE w_data-taxable_county  TO UPPER CASE.
          TRANSLATE w_data-taxable_city TO UPPER CASE.

          APPEND w_data TO t_data.

        ENDLOOP.
      ENDIF.
    ENDIF.

    IF t_data[] IS NOT INITIAL.
      APPEND LINES OF t_data FROM 1 TO g_packagesize TO e_t_data.
      DELETE t_data FROM 1 TO g_packagesize.
*    ELSEIF e_t_data[] IS INITIAL AND t_bkpf[] IS INITIAL."Del MOD-001
    ELSEIF e_t_data[] IS INITIAL AND t_doc_ext[] IS INITIAL."Ins MOD-001
      CLOSE CURSOR g_dpext.
      RAISE no_more_data.
    ENDIF.

  ENDFUNCTION.
