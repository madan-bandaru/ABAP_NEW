*&---------------------------------------------------------------------*
*&      Form  sub_authority_check
*&---------------------------------------------------------------------*
*       For authority check
*----------------------------------------------------------------------*
FORM sub_authority_check  USING    fp_im_company_code TYPE bukrs
                                   fp_t_tabrec        TYPE x_ty_tabrec
                                   ft_tocde           TYPE tcode
                          CHANGING fp_w_comp_codes    TYPE x_bukrs
                                   fp_t_comp_codes    TYPE x_ty_bukrs
                                   fp_w_tabrec   TYPE /ds1/fi_ms_sapgen
                                   fp_w_result_messages TYPE balmt
                                   fp_t_result_messages TYPE x_ty_balmt
                                   fp_g_flag            TYPE char1.

  CLEAR fp_w_result_messages.
* Check whether the user has authority for transaction FBV1
  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = ft_tocde
    EXCEPTIONS
      ok     = 1
      not_ok = 2
      OTHERS = 3.
  IF sy-subrc > 1.
    MOVE 'Not Authorised for transaction FBV1'(e01)
               TO fp_w_result_messages-msgtxt.

    APPEND fp_w_result_messages TO fp_t_result_messages.
    fp_g_flag = c_x.
    EXIT.
  ENDIF.
  fp_w_comp_codes-bukrs = fp_im_company_code.
  APPEND fp_w_comp_codes TO fp_t_comp_codes.
  CLEAR fp_w_comp_codes.
  LOOP AT fp_t_tabrec INTO fp_w_tabrec.
    fp_w_comp_codes-bukrs = fp_w_tabrec-newbk.
    TRANSLATE fp_w_comp_codes-bukrs TO UPPER CASE.   "Defect 6168
    APPEND fp_w_comp_codes TO fp_t_comp_codes.
  ENDLOOP.
  LOOP AT fp_t_comp_codes INTO fp_w_comp_codes.
    AUTHORITY-CHECK OBJECT c_f_bkpf_buk
                    ID c_bukrs FIELD fp_w_comp_codes-bukrs
                    ID c_actvt FIELD c_01.
    IF sy-subrc IS NOT INITIAL.
      CONCATENATE 'Not Authorised for'(002)
                 fp_w_comp_codes-bukrs INTO fp_w_result_messages-msgtxt
                 SEPARATED BY space.
      APPEND fp_w_result_messages TO fp_t_result_messages.
      fp_g_flag = c_x.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " sub_authority_check

*&---------------------------------------------------------------------*
*&      Form  sub_format
*&---------------------------------------------------------------------*
*       To change the format of date and amount fields
*----------------------------------------------------------------------*
FORM sub_format  CHANGING fp_im_document_date TYPE budat
                          fp_im_posting_date  TYPE budat
                          fp_im_translation_date TYPE budat.
*                          fp_im_reversal_date TYPE budat.


  IF g_dat_fmt IS NOT INITIAL.
    PERFORM sub_dat_change USING    g_dat_fmt
                           CHANGING fp_im_document_date.
    PERFORM sub_dat_change USING    g_dat_fmt
                           CHANGING fp_im_posting_date.
    PERFORM sub_dat_change USING    g_dat_fmt
                           CHANGING fp_im_translation_date.
  ENDIF.
ENDFORM.                    " sub_format
*&---------------------------------------------------------------------*
*&      Form  sub_dat_change
*&---------------------------------------------------------------------*
*       Changing date format
*----------------------------------------------------------------------*
FORM sub_dat_change  USING    fp_dat_fmt          TYPE xudatfm
                     CHANGING fp_date             TYPE budat.

  DATA: l_dd TYPE char2,
        l_mm TYPE char2,
        l_yy TYPE char4.

  l_dd    = fp_date+6(2).
  l_mm    = fp_date+4(2).
  l_yy    = fp_date(4).

  CASE fp_dat_fmt.
*-Changing date according to selected format
    WHEN c_one.                      " JJ.MM.AAAA
      CONCATENATE l_dd l_mm l_yy INTO fp_date.

    WHEN c_two OR c_three.           " MM.JJ.AAAA
      CONCATENATE l_mm l_dd l_yy INTO fp_date.

    WHEN c_four OR c_five OR c_six.  " AAAA.MM.JJ
      CONCATENATE l_yy l_mm l_dd INTO fp_date.
    WHEN '7' OR '8' OR '9' OR 'A' OR 'B' OR 'C'.

  ENDCASE.

  CONDENSE fp_date.

ENDFORM.                    " sub_dat_change

*&---------------------------------------------------------------------*
*&      Form  F_INITIALIZE_STRUCT
*&---------------------------------------------------------------------*
*       Initialize structure BBKPF & BBSEG with the forward slashes
*----------------------------------------------------------------------*
*      <--P_STRUCT  Structure ( BBKPF OR BBSEG )
*----------------------------------------------------------------------*
FORM f_initialize_struct CHANGING fp_struct TYPE any.

* Local data declaration.
  FIELD-SYMBOLS : <fs_field> TYPE any. "For referring a field

  WHILE ( 0 = 0 ).
    ASSIGN COMPONENT sy-index OF STRUCTURE fp_struct TO <fs_field>.
    IF sy-subrc = 0.
      IF <fs_field> IS INITIAL.
* Assign forward slash to all fields of structure.
        <fs_field> = c_slash.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDWHILE.

ENDFORM.                    " f_initialize_struct

*&---------------------------------------------------------------------*
*&      Form  SUB_FORMAT_REV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_REVERSAL_DATE  text
*----------------------------------------------------------------------*
FORM sub_format_rev  CHANGING fp_im_reversal_date TYPE budat.
  DATA: l_dat_fmt TYPE xudatfm.

*-Selecting date format based on user name
  SELECT SINGLE datfm
         INTO l_dat_fmt
         FROM usr01
         WHERE bname = sy-uname.
  IF sy-subrc IS INITIAL.
    PERFORM sub_dat_change USING    l_dat_fmt
                           CHANGING fp_im_reversal_date.
  ENDIF.
ENDFORM.                    " SUB_FORMAT_REV
*&---------------------------------------------------------------------*
*&      Form  GET_DAT_FMT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dat_fmt .
*-Selecting date format based on user name
  SELECT SINGLE datfm
                dcpfm
         INTO (g_dat_fmt , g_dcm_fmt)
         FROM usr01
         WHERE bname = sy-uname.
ENDFORM.                    " GET_DAT_FMT
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_BUKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_bukrs USING fp_bukrs TYPE bukrs .
  DATA:l_bukrs TYPE bukrs.
  SELECT SINGLE bukrs
    FROM t001
    INTO l_bukrs
   WHERE bukrs = fp_bukrs.
  IF sy-subrc NE 0.
    g_flag = c_x.
  ENDIF.
ENDFORM.                    " VALIDATE_BUKRS
*&---------------------------------------------------------------------*
*&      Form  DELETE_BATCH_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_batch_input .
  CLEAR : g_jobnam .
  CONCATENATE 'Delete_Batch' g_jobnam1 INTO g_jobnam.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = g_jobnam
    IMPORTING
      jobcount         = g_jobcount
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
*---Failed to create batch job for parking &
*    MESSAGE i070 WITH g_jobnam .

    LEAVE LIST-PROCESSING.
  ENDIF.
  SUBMIT rsbdcreo WITH mappe = g_jobnam1
                  WITH from-dat = g_date
                  WITH from-tim = g_uzeit
                  WITH to-dat = space
                  WITH to-tim = space
                  WITH period = space
                  WITH mappen = 'X'
                  WITH protokol = 'X'
                  USER sy-uname
                  VIA JOB g_jobnam NUMBER g_jobcount AND RETURN.

  IF sy-subrc <> 0.
*---Job could not be processed for program RFBIBL00.
*    MESSAGE i072.

    LEAVE LIST-PROCESSING.
  ENDIF.

* Close the Job scheduling
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount             = g_jobcount       " job count
      jobname              = g_jobnam         " job name
      strtimmed            = c_x                 " strimmed
*    IMPORTING
*     job_was_released     =
    EXCEPTIONS
      cant_start_immediate = 1
      invalid_startdate    = 2
      jobname_missing      = 3
      job_close_failed     = 4
      job_nosteps          = 5
      job_notex            = 6
      lock_failed          = 7
      OTHERS               = 8.

ENDFORM.                    " DELETE_BATCH_INPUT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_LONGTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_longtext USING fp_bukrs TYPE bukrs
                           fp_date TYPE budat.
  LOOP AT t_longtext INTO w_longtext.
    CLEAR w_thead.
    REFRESH:t_tline[].
    CONCATENATE fp_bukrs
                g_docno
                fp_date+4(4)
                w_longtext-item
           INTO g_tdname.

    w_tline-tdformat = '*'.
    w_tline-tdline = w_longtext-text.
    APPEND w_tline TO t_tline.
    CLEAR w_tline.

    w_thead-tdobject = 'DOC_ITEM'.
    w_thead-tdname   = g_tdname.
    w_thead-tdid     = '0001'.
    w_thead-tdspras  = 'E'.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client          = sy-mandt
        header          = w_thead
        insert          = 'X'
        savemode_direct = 'X'
      IMPORTING
        newheader       = w_thead
      TABLES
        lines           = t_tline
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'COMMIT_TEXT'
        EXPORTING
          savemode_direct = 'X'.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " UPDATE_LONGTEXT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SUNDRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_COMPANY_CODE  text
*      -->P_POSTING_DATE  text
*----------------------------------------------------------------------*
FORM update_sundry  USING   fp_bukrs TYPE bukrs
                            fp_date TYPE budat
                            fp_longtext TYPE text120.
  IF g_docno IS NOT INITIAL.
    CLEAR w_thead.
    CONCATENATE fp_bukrs
                g_docno
                fp_date+4(4)
           INTO g_tdname.

    w_tline-tdformat = '*'.
    w_tline-tdline = fp_longtext.
    APPEND w_tline TO t_tline.
    CLEAR w_tline.

    w_thead-tdobject = 'BELEG'.
    w_thead-tdname   = g_tdname.
    w_thead-tdid     = 'Z011'.
    w_thead-tdspras  = 'E'.
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client          = sy-mandt
        header          = w_thead
        insert          = 'X'
        savemode_direct = 'X'
      IMPORTING
        newheader       = w_thead
      TABLES
        lines           = t_tline
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'COMMIT_TEXT'
        EXPORTING
          savemode_direct = 'X'.
    ENDIF.
  ENDIF.
ENDFORM.                    " UPDATE_SUNDRY
*&---------------------------------------------------------------------*
*&      Form  GET_DCM_FMT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_W_SAPGEN_WRBTR  text
*----------------------------------------------------------------------*
FORM get_dcm_fmt  CHANGING fp_wrbtr TYPE wrbtr_bi
                           fp_dmbtr TYPE dmbtr_bi
                           fp_dmbe2 TYPE wrbtr_bi
                           fp_wmwst  TYPE wmwst_bi.
  CASE g_dcm_fmt.
    WHEN 'Y' OR ' '.
      REPLACE  '.' WITH ',' INTO fp_wrbtr .
      REPLACE  '.' WITH ',' INTO fp_dmbtr .
      REPLACE  '.' WITH ',' INTO fp_dmbe2 .
      REPLACE  '.' WITH ',' INTO fp_wmwst .
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " GET_DCM_FMT
