FUNCTION ZFSSI1454_C_FB01_POST.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(POSTING_DATE) TYPE  BLDAT OPTIONAL
*"     VALUE(DOCUMENT_DATE) TYPE  BLDAT OPTIONAL
*"     VALUE(CURRENCY) TYPE  WAERS OPTIONAL
*"     VALUE(DOCUMENT_TYPE) TYPE  BLART OPTIONAL
*"     VALUE(COMPANY_CODE) TYPE  BUKRS OPTIONAL
*"     VALUE(POSTING_PERIOD) TYPE  MONAT OPTIONAL
*"     VALUE(TRANSLATION_DATE) TYPE  BLDAT OPTIONAL
*"     VALUE(EXCHANGE_RATE) TYPE  KURSF OPTIONAL
*"     VALUE(TRADING_PARTNER) TYPE  VBUND OPTIONAL
*"     VALUE(SESSION_KEEP_OPTION) TYPE  APQ_QDEL DEFAULT ' '
*"     VALUE(SESSION_NAME) TYPE  APQ_GRPN DEFAULT 'SAPGen_Excel'
*"     VALUE(REFERENCE_DOC_NUMBER) TYPE  XBLNR OPTIONAL
*"     VALUE(HEADER_TEXT) TYPE  BKTXT OPTIONAL
*"     VALUE(CALC_TAX_SWITCH) TYPE  XMWST OPTIONAL
*"     VALUE(LONG_TEXT) TYPE  TEXT120 OPTIONAL
*"     VALUE(IM_CHECK) TYPE  CHAR1 OPTIONAL
*"     VALUE(IM_RELEASE) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     VALUE(RETURN_STATUS) TYPE  CHAR1
*"  TABLES
*"      TABREC STRUCTURE  /DS1/FI_MS_SAPGEN
*"      RESULT_MESSAGES STRUCTURE  BALMT
*"--------------------------------------------------------------------
* DATE CHANGE.....:  25-06-2015                                        *
* AUTHOR..........:  Roshni Raveendran(INRRGY)                         *
* CHANGE DESCR....:  Added the posting key 29 for SAPGEN               *
* MODIFICATION ID :  MOD-003                                           *
* SERVICE CENT REF:  GIM32280019                                       *
* RT/TRANSPORT....:  82910/D94K9A6NIA                                  *
************************************************************************
************************************************************************
* DATE CHANGE.....:  21-12-2016                                        *
* AUTHOR..........:  Mallaiah Yamani (INBYA3)                          *
* CHANGE DESCR....:  Adding posting keys 39 and 26 to SAPGEN for CN04  *
* MODIFICATION ID :  MOD-004                                           *
* CHANGE REQUEST  :  55826                                             *
* RT/TRANSPORT....:  90546/D94K9A79AM                                  *
************************************************************************

  DATA: l_saknr       TYPE saknr,
        l_tabix       TYPE sy-tabix,
        l_posting_key TYPE newbs,
        l_item_count  TYPE sytabix,
        l_total_amt   TYPE wrbtr,
        l_wrbtr       TYPE wrbtr,
        l_wmwst       TYPE wmwst.

  REFRESH result_messages[].
  CLEAR: w_result_messages,
         return_status.

  LOOP AT tabrec INTO w_sapgen.
    g_index = sy-tabix.
    IF w_sapgen-newbs = '01' OR
       w_sapgen-newbs = '02' OR
       w_sapgen-newbs = '04' OR
       w_sapgen-newbs = '11' OR
       w_sapgen-newbs = '19' OR
       w_sapgen-newbs = '21' OR
       w_sapgen-newbs = '26' OR  "MOD-004
       w_sapgen-newbs = '29' OR
       w_sapgen-newbs = '31' OR
       w_sapgen-newbs = '32' OR
       w_sapgen-newbs = '39' OR  "MOD-004
       w_sapgen-newbs = '40' OR
       w_sapgen-newbs = '50'.

    ELSE.
      w_result_messages-mandant = sy-mandt.
      w_result_messages-langu = sy-langu.
      w_result_messages-msgty = c_e.        " Defect 333708
      CONCATENATE 'Posting key' w_sapgen-newbs 'in line' g_index 'not allowed'
             INTO g_msg SEPARATED BY space.
      w_result_messages-msgtxt = g_msg.
      APPEND w_result_messages TO result_messages.
      CLEAR w_result_messages.
      return_status = 'N'.         " Defect 333708
    ENDIF.
  ENDLOOP.
  IF result_messages[] IS NOT INITIAL.
    EXIT.
  ENDIF.

  PERFORM validate_bukrs USING company_code.
  IF g_flag = c_x.
    w_result_messages-mandant = sy-mandt.
    w_result_messages-langu = sy-langu.
    w_result_messages-msgty = c_e.        " Defect 333708
    w_result_messages-msgtxt = 'Invalid company code entered'.
    APPEND w_result_messages TO result_messages.
    CLEAR w_result_messages.
    return_status = 'N'.         " Defect 333708
    EXIT.
  ENDIF.

  PERFORM get_dat_fmt.

*-Authority check to allow only users authorised to use this SAPGen
* utility
  PERFORM sub_authority_check USING    company_code
                                       tabrec[]
                                       'FB01'
                              CHANGING w_comp_codes
                                       t_comp_codes
                                       w_tabrec
                                       w_result_messages
                                       result_messages[]
                                       g_flag.
  IF g_flag = c_x.
    w_result_messages-mandant = sy-mandt.
    w_result_messages-langu   = sy-langu.
    w_result_messages-msgty = c_e.        " Defect 333708
    w_result_messages-msgtxt  = 'No Authorization for Company Code'.
    APPEND w_result_messages TO result_messages.
    CLEAR w_result_messages.
    return_status = 'N'.         " Defect 333708
    EXIT.
  ENDIF.

*-To convert the date and amount format
  PERFORM sub_format CHANGING document_date
                              posting_date
                              translation_date.

  w_bgr00-stype = 0.
  w_bgr00-group = session_name.
  w_bgr00-mandt = sy-mandt.
  w_bgr00-usnam = sy-uname.
  w_bgr00-start = sy-datum - 1.
  w_bgr00-xkeep = session_keep_option.
  w_bgr00-nodata = c_slash.
  w_bbkpf-stype = 1.
  w_bbkpf-tcode = 'FB01'.
  w_bbkpf-bldat = document_date.
  w_bbkpf-blart = document_type.
  w_bbkpf-bukrs = company_code.
  w_bbkpf-budat = posting_date.
  w_bbkpf-monat = posting_period.
  w_bbkpf-waers = currency.
  w_bbkpf-xblnr = reference_doc_number.
  w_bbkpf-bktxt = header_text.
  w_bbkpf-xbwae = ''.

  IF exchange_rate IS NOT INITIAL.
    WRITE exchange_rate TO g_value.
    w_bbkpf-kursf = g_value.
  ENDIF.
*Begin of defect 276291
  TRANSLATE calc_tax_switch TO UPPER CASE.
*End of defect 276291
  w_bbkpf-xmwst = calc_tax_switch.
  w_bbkpf-vbund = trading_partner.

  IF translation_date IS NOT INITIAL.
    w_bbkpf-wwert = translation_date.
  ENDIF.

  SELECT * FROM t001 INTO w_t001
                WHERE bukrs = company_code.
  ENDSELECT.

  SELECT * FROM t001a INTO w_t001a
                WHERE bukrs = company_code.
  ENDSELECT.

  DESCRIBE TABLE tabrec LINES g_tabix.  " MOD-001
  IF g_tabix LT c_max_items.            " MOD-001
    l_tabix = g_tabix.                  " MOD-001
  ENDIF.                                " MOD-001

  LOOP AT tabrec INTO w_sapgen.
    ADD c_one TO l_item_count.   " MOD-001

    IF w_sapgen-long_text IS NOT INITIAL.
      g_item = sy-tabix.
      w_longtext-item = g_item.
      w_longtext-text = w_sapgen-long_text.
      APPEND w_longtext TO t_longtext.
      CLEAR w_longtext.
    ENDIF.
*-To convert the date and amount format
    PERFORM sub_format CHANGING w_sapgen-bzdat
                                w_sapgen-valut
                                w_sapgen-zfbdt.

    w_bbseg-stype = 2.
    w_bbseg-tbnam = 'BBSEG'.
    MOVE-CORRESPONDING w_sapgen TO w_bbseg.
    w_bbseg-bewar = w_sapgen-rmvct.
    w_bbseg-rke_vsbed = w_sapgen-vsbed.
    w_bbseg-rke_ktgrd = w_sapgen-ktgrd.

    IF NOT w_sapgen-amtlc IS INITIAL.
      w_bbseg-dmbtr = w_sapgen-amtlc.
    ENDIF.

    IF w_sapgen-ergoknot = 'X'.
      w_bbseg-rke_prctr = w_bbseg-prctr.
      w_bbseg-rke_bukrs = company_code.
      w_bbseg-rke_prctr = w_bbseg-prctr.
      w_bbseg-rke_werks = w_bbseg-werks.
      w_bbseg-rke_vkorg = w_sapgen-vkorg.
      w_bbseg-rke_vtweg = w_sapgen-vtweg.
      w_bbseg-rke_spart = w_sapgen-spart.
      w_bbseg-rke_kndnr = w_sapgen-kndnr.
      w_bbseg-rke_kunwe = w_sapgen-kunwe.
    ENDIF.
    IF currency = 'OMR' OR currency = 'omr'.
      w_bbseg-wrbtr = w_bbseg-wrbtr * 10.
      w_bbseg-dmbe2 = w_bbseg-dmbe2 * 10.
      w_bbseg-dmbtr = w_bbseg-dmbtr * 10.
    ENDIF.

    l_total_amt = l_total_amt + w_sapgen-wrbtr  " MOD-001
                  + w_sapgen-wmwst. " INRYA4

    g_amount = w_bbseg-wrbtr.
    IF g_amount LT 0.
      w_bbseg-wrbtr = g_amount * -1.
    ENDIF.

    g_amount = w_bbseg-dmbe2.
    IF g_amount LT 0.
      w_bbseg-dmbe2 = g_amount * -1.
    ENDIF.

    g_amount = w_bbseg-dmbtr.
    IF g_amount LT 0.
      w_bbseg-dmbtr = g_amount * -1.
    ENDIF.

    IF currency = 'OMR' OR currency = 'omr'.
      w_bbseg-wrbtr = w_bbseg-wrbtr / 10.
      w_bbseg-dmbe2 = w_bbseg-dmbe2 / 10.
      w_bbseg-dmbtr = w_bbseg-dmbtr / 10.
    ENDIF.

*Defect 253532
    PERFORM get_dcm_fmt CHANGING w_bbseg-wrbtr
                                 w_bbseg-dmbtr
                                 w_bbseg-dmbe2
                                 w_bbseg-wmwst.
*Defect 253532

    IF currency = w_t001-waers.
      CLEAR: w_bbseg-dmbtr.
    ENDIF.

    IF currency = 'USD' AND
       w_t001a-curtp = '30'.
      CLEAR:w_bbseg-dmbe2.
    ENDIF.

    IF w_bbseg-bzdat EQ '00000000'.
      CLEAR w_bbseg-bzdat.
    ENDIF.

    IF w_bbseg-zfbdt EQ '00000000'.
      CLEAR w_bbseg-zfbdt.
    ENDIF.

    IF w_bbseg-valut EQ '00000000'.
      CLEAR w_bbseg-valut.
    ENDIF.
* BOI INUJA5
    IF w_sapgen-newum IS NOT INITIAL.
      w_bbseg-newum = w_sapgen-newum.
    ENDIF.
* EOI INUJA5
*Begin of Defect ID : 276293 SAPGEN - Partner Profit Centre not picked up in document
    IF w_sapgen-pprctr IS NOT INITIAL.
      w_bbseg-pprct = w_sapgen-pprctr.
    ENDIF.
*End of Defect ID : 276293 SAPGEN - Partner Profit Centre not picked up in document
*Begin of D94K9A5H3H - change to update RE-fields HPQC Req 185436
    IF     w_sapgen-smenr IS NOT INITIAL OR
           w_sapgen-swenr IS NOT INITIAL OR
           w_sapgen-sgenr IS NOT INITIAL OR
           w_sapgen-sgrnr IS NOT INITIAL.

      w_bbseg-wenr = w_sapgen-swenr.
      w_bbseg-genr = w_sapgen-sgenr.
      w_bbseg-grnr = w_sapgen-sgrnr.
      w_bbseg-menr = w_sapgen-smenr.
    ENDIF.

*End of D94K9A5H3H change to update RE-fields HPQC Req 185436
*Begin of Defect ID : 276904 SAPGEN - formatting error with Quantity column when postings made via SAPGEN
    IF w_bbseg-menge IS NOT INITIAL.
      IF g_dcm_fmt = 'Y' OR g_dcm_fmt = ' '.
        TRANSLATE w_bbseg-menge USING lc_dots.
      ENDIF.
    ENDIF.
    IF  w_bbseg-newbs = '11'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = w_bbseg-newko
        IMPORTING
          output = l_kunnr.

      w_bbseg-newko = l_kunnr.
      CLEAR:l_kunnr.
    ENDIF.
*End of Defect ID : 276904 SAPGEN - formatting error with Quantity column when postings made via SAPGEN
    PERFORM f_initialize_struct CHANGING w_bbseg.
    APPEND w_bbseg TO t_bbseg.
*Defect 276293
    IF  w_bbseg-newbs = '11'.
      w_bseg_temp-newko = w_bbseg-newko.
      APPEND w_bseg_temp TO t_bseg_temp.
      CLEAR: w_bseg_temp,l_kunnr.
    ENDIF.
*End of defect 276293
    CLEAR w_bbseg.

    " Begin of MOD-001

    IF l_item_count GT c_max_items OR
      l_item_count EQ l_tabix.

      IF l_total_amt NE c_null_value.
        IF l_total_amt LT c_null_value.
          l_posting_key = c_clearing_debit_post_key.
          l_total_amt = l_total_amt * c_negative.
        ELSE.
          l_posting_key = c_clearing_credit_post_key.
          l_total_amt = l_total_amt * 1.
        ENDIF.
        w_bbseg-stype = 2.
        w_bbseg-tbnam = 'BBSEG'.
        w_bbseg-newbs = l_posting_key.
        w_bbseg-newko = c_clearing_gl_account.
        w_bbseg-wrbtr = l_total_amt.
        PERFORM get_dcm_fmt CHANGING w_bbseg-wrbtr
                                w_bbseg-dmbtr
                                w_bbseg-dmbe2
                                w_bbseg-wmwst.
        PERFORM f_initialize_struct CHANGING w_bbseg.
        APPEND w_bbseg TO t_bbseg.

        CLEAR w_bbseg.
      ENDIF.
      " End of MOD-001

*Begin of defect - 276293
      IF t_bseg_temp[] IS NOT INITIAL.
        SORT t_bseg_temp BY newko.
        SELECT
              kunnr
              bukrs
              witht
              wt_withcd
              wt_agent
        FROM knbw
        INTO TABLE t_knbw
        FOR ALL ENTRIES IN t_bseg_temp
        WHERE bukrs = company_code AND
              kunnr = t_bseg_temp-newko AND
              wt_agent = 'X'.

        SORT t_knbw BY bukrs kunnr.
        DELETE ADJACENT DUPLICATES FROM t_knbw COMPARING bukrs kunnr.
      ENDIF.
*End of defect - 276293
* Initialize structures
      PERFORM f_initialize_struct CHANGING w_bgr00.
      PERFORM f_initialize_struct CHANGING w_bbkpf.
*      CLEAR:w_bbkpf-xbwae.   " INRYA4

      CONCATENATE sy-uname sy-datum sy-uzeit 'FB01.txt' INTO g_filename SEPARATED BY '_'.
      CONCATENATE c_folder syst-sysid c_folder1 g_filename INTO g_filename SEPARATED BY '/'.

      OPEN DATASET g_filename FOR OUTPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS.
      IF sy-subrc EQ 0.
        TRANSFER w_bgr00 TO g_filename.
        TRANSFER w_bbkpf TO g_filename.

        LOOP AT t_bbseg INTO w_bbseg.
          TRANSFER w_bbseg TO g_filename.
** Begin of WTH Tax Structure data: ECC 6.0
          READ TABLE t_knbw INTO w_knbw WITH KEY kunnr = w_bbseg-newko.
          IF sy-subrc = 0.
            w_bwith-stype = '2'.
            w_bwith-tbnam = 'BWITH'.
            w_bwith-witht = 'X'.
            w_bwith-wt_withcd = 'XX'.
*      w_bwith-wt_qsshb  = '100.00'. "
*      w_bwith-wt_qbuihb = 'XX'. "
**    Base to calculate the Withholding tax value
**  WRITE <base amount> CURRENCY BBKPF-WAERK TO BWITH-WT_QSSHB.
**The Withholding tax codes have to picked up from vendor table LFBK,
**the base amount has to be calculate (it depends on the percentage stored in T059Z):
**<base amount> = <amount> * T059Z-QPROZ / 100
            TRANSFER w_bwith TO g_filename.
          ENDIF.
** End of WTH Tax Structure data: ECC 6.0
        ENDLOOP.
      ELSE.
        w_result_messages-mandant = sy-mandt.
        w_result_messages-langu = sy-langu.
        w_result_messages-msgty = c_e.        " Defect 333708
        w_result_messages-msgtxt = 'No Authorization to write the File'..
        APPEND w_result_messages TO result_messages.
        CLEAR w_result_messages.
        return_status = 'N'.         " Defect 333708
        RETURN.
      ENDIF.
* submit the transaction.

* Close the file.
      CLOSE DATASET g_filename.
      CLEAR: w_bbseg. " w_bbkpf w_bgr00.

      GET TIME.
      g_date  = sy-datum.
      g_uzeit = sy-uzeit.

* Justification: It is convention followed in FI posting.
      SUBMIT rfbibl00
                    WITH callmode = 'B'
                    WITH ds_name  = g_filename
                    WITH max_comm = '99'
                    WITH xinf = c_x
                     AND RETURN.

      IF sy-subrc <> 0.
*---Job could not be processed for program RFBIBL00.
        w_result_messages-mandant = sy-mandt.
        w_result_messages-langu = sy-langu.
        w_result_messages-msgty = c_e.        " Defect 333708
        w_result_messages-msgtxt = text-001.
        APPEND w_result_messages TO result_messages.
        CLEAR w_result_messages.
        return_status = 'N'.
        EXIT.
      ENDIF.
      l_tabix = g_tabix - l_item_count.
      l_item_count = c_zero.
      CLEAR l_total_amt.
      REFRESH  t_bbseg.

      CLEAR : g_status .

      DELETE DATASET g_filename.  " INUJA5
*      IF im_check IS NOT INITIAL. " INUJA5
      IF im_release IS NOT INITIAL. " INUJA5
        CONCATENATE g_jobnam sy-uzeit INTO g_jobnam.

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
          w_result_messages-mandant = sy-mandt.
          w_result_messages-langu = sy-langu.
          w_result_messages-msgty = c_e.        " Defect 333708
          w_result_messages-msgtxt = text-002.
          APPEND w_result_messages TO result_messages.
          CLEAR w_result_messages.
          return_status = 'R'.
          EXIT.
        ENDIF.

* Process the batch job using program RSBDCSUB
        SUBMIT rsbdcsub WITH mappe = session_name        "#EC CI_SUBMIT
                        WITH fehler = space
                        WITH z_verarb = 'X'
                        WITH von   = sy-datum
                        USER sy-uname
                         VIA JOB g_jobnam NUMBER g_jobcount AND RETURN.

        IF sy-subrc <> 0.
*---Job could not be processed for program RFBIBL00.
          w_result_messages-mandant = sy-mandt.
          w_result_messages-langu = sy-langu.
          w_result_messages-msgty = c_e.        " Defect 333708
          w_result_messages-msgtxt = text-002.
          CONCATENATE session_name w_result_messages-msgtxt INTO w_result_messages-msgtxt SEPARATED BY ' '.
          APPEND w_result_messages TO result_messages.
          CLEAR w_result_messages.
          return_status = 'R'.
          EXIT.
        ENDIF.

* Close the Job scheduling
        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = g_jobcount       " job count
            jobname              = g_jobnam         " job name
            strtimmed            = c_x                 " strimmed
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            OTHERS               = 8.

        IF sy-subrc <> 0.
*---Job could not close &
          w_result_messages-mandant = sy-mandt.
          w_result_messages-langu = sy-langu.
          w_result_messages-msgtxt = text-002.
          w_result_messages-msgty = c_e.        " Defect 333708
          CONCATENATE session_name w_result_messages-msgtxt INTO w_result_messages-msgtxt SEPARATED BY ' '.
          APPEND w_result_messages TO result_messages.
          CLEAR w_result_messages.
          return_status = 'R'.
          EXIT.
        ENDIF.

        g_jobnam1 = session_name.

*   Get data for Queue info definition
        SELECT mandant                                  "#EC CI_NOFIRST
               groupid
               qid
               qstate
               credate
               cretime
              INTO TABLE t_lat_ses BYPASSING BUFFER
                  FROM apqi
                  WHERE mandant = sy-mandt
                  AND   groupid = session_name
                  AND   creator = sy-uname
                  AND   credate = sy-datum.

        IF sy-subrc <> 0.
          w_result_messages-mandant = sy-mandt.
          w_result_messages-langu = sy-langu.
          w_result_messages-msgtxt = text-003.
          CONCATENATE session_name w_result_messages-msgtxt INTO w_result_messages-msgtxt SEPARATED BY ' '.
          APPEND w_result_messages TO result_messages.
          CLEAR w_result_messages.
          return_status = 'T'.
          EXIT.
        ENDIF.

        DELETE t_lat_ses WHERE cretime < g_uzeit.
* Get latest session QID
        SORT t_lat_ses DESCENDING BY credate cretime.
        DELETE ADJACENT DUPLICATES FROM t_lat_ses COMPARING groupid.

        IF t_lat_ses[] IS NOT INITIAL.
*   Get TEMSEID of the selected Queue ID
          READ TABLE t_lat_ses INTO w_lat_ses INDEX 1.
          IF sy-subrc = 0 .
            DO 30 TIMES.

              SELECT
                  temseid
                  credate
                  cretime
                  groupid
                  qid
                  status
                   INTO  TABLE t_temse
                     FROM apql BYPASSING BUFFER
                     WHERE mandant = sy-mandt
                         AND credate = sy-datum
                         AND groupid = session_name
                         AND qid     = w_lat_ses-qid
                         AND creator = sy-uname.        "#EC CI_NOFIRST

              IF sy-subrc EQ 0.
*---TEMSEID not available for the batch input session &
                SORT t_temse BY credate DESCENDING cretime.
                CLEAR w_temse.
*---Retrieve Queue status for session
                READ TABLE t_temse INTO w_temse INDEX 1.
                IF w_temse-status = 'W'."Session still running
                  WAIT UP TO 2 SECONDS.
                  CONTINUE.
                ENDIF.
                EXIT.
              ELSE.
                WAIT UP TO 2 SECONDS."Session not yet registered
              ENDIF.
            ENDDO.
          ENDIF.
* BOI Defect 334668
        ELSE.
*--Job could not be processed for program RFBIBL00.
          w_result_messages-mandant = sy-mandt.
          w_result_messages-langu = sy-langu.
          w_result_messages-msgty = c_e.        " Defect 333708
          w_result_messages-msgtxt = text-001.
          APPEND w_result_messages TO result_messages.
          CLEAR w_result_messages.
          return_status = 'N'.
          EXIT.
* EOI Defect 334668
        ENDIF.

        IF  t_temse IS INITIAL.
          w_result_messages-mandant = sy-mandt.
          w_result_messages-langu = sy-langu.
          w_result_messages-msgtxt = text-003.
          CONCATENATE session_name w_result_messages-msgtxt INTO w_result_messages-msgtxt SEPARATED BY ' '.
          APPEND w_result_messages TO result_messages.
          CLEAR w_result_messages.
          return_status = 'T'.
          EXIT.
        ELSE.
          READ TABLE t_temse INTO w_temse INDEX 1.
          IF w_temse-status = 'W'."If the Session is still running at this point then message to check SM35
            w_result_messages-mandant = sy-mandt.
            w_result_messages-langu = sy-langu.
            w_result_messages-msgtxt = text-004.
            CONCATENATE session_name w_result_messages-msgtxt INTO w_result_messages-msgtxt SEPARATED BY ' '.
            APPEND w_result_messages TO result_messages.
            CLEAR w_result_messages.
            return_status = 'T'.
            session_keep_option = 'X'.
            IF l_tabix EQ 0.     "In case of more than 999 items do not exit SAPGEN as few more line items are pending
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
        IF w_temse-status NE 'W'."If the Session is still running do not read SM35 log
*   Call function module 'RSTS_OPEN_RLC' for
*   Opening TemSe Object To Read Line By Line With Conversion
          IF sy-subrc = 0.
            CALL FUNCTION 'RSTS_OPEN_RLC'
              EXPORTING
                name           = w_temse-temseid
                client         = sy-mandt
                authority      = c_auth
                prom           = c_prom
                rectyp         = c_rectyp
*               enqlock        = c_e
              EXCEPTIONS
                fb_call_handle = 4
                fb_error       = 8
                fb_rsts_noconv = 12
                fb_rsts_other  = 16
                no_object      = 20
                OTHERS         = 24.
            IF sy-subrc NE 0.
              w_result_messages-mandant = sy-mandt.
              w_result_messages-langu = sy-langu.
              w_result_messages-msgtxt = text-003.
              APPEND w_result_messages TO result_messages.
              CLEAR w_result_messages.
              return_status = 'T'.
              EXIT.
            ENDIF.
*     Call function module 'RSTS_READ' for
*     Reading from TemSe Object

            CALL FUNCTION 'RSTS_READ'
              TABLES
                datatab        = t_session_log
              EXCEPTIONS
                fb_call_handle = 4
                fb_error       = 8
                fb_rsts_noconv = 12
                fb_rsts_other  = 16
                OTHERS         = 16.

            IF sy-subrc NE 0.
              w_result_messages-mandant = sy-mandt.
              w_result_messages-langu = sy-langu.
              w_result_messages-msgtxt = text-003.
              CONCATENATE session_name w_result_messages-msgtxt INTO w_result_messages-msgtxt SEPARATED BY ' '.
              APPEND w_result_messages TO result_messages.
              CLEAR w_result_messages.
              return_status = 'T'.
              EXIT.
            ENDIF.

*    Call function module 'RSTS_CLOSE' for
*    Closing TemSe Object Again
            CALL FUNCTION 'RSTS_CLOSE'
              EXCEPTIONS
                OTHERS = 4.
            IF sy-subrc NE 0.
              w_result_messages-mandant = sy-mandt.
              w_result_messages-langu = sy-langu.
              w_result_messages-msgtxt = text-003.
              CONCATENATE session_name w_result_messages-msgtxt INTO w_result_messages-msgtxt SEPARATED BY ' '.
              APPEND w_result_messages TO result_messages.
              CLEAR w_result_messages.
              return_status = 'T'.
              EXIT.
            ENDIF.

            t_session_log_tmp[] = t_session_log[].
            DELETE t_session_log_tmp WHERE logmessage+94(3)  EQ '300'.
          ENDIF.

          g_temp = 1.
* Find out if there are any sessions in error.
          LOOP AT t_session_log INTO w_session_log WHERE
                                           logmessage+73(1) = c_e.
            g_status1  = c_f.
            w_bdclm = w_session_log.

            g_msg_id = w_bdclm-mid.
            g_msg_nr = w_bdclm-mnr.

            IF strlen( w_bdclm-mpar ) NE 0 .
              g_len = w_bdclm-mpar+0(2).
              IF g_len > 0 .
                SHIFT  w_bdclm-mpar BY 2 PLACES.
                g_msg1 = w_bdclm-mpar+0(g_len).
                SHIFT  w_bdclm-mpar BY g_len PLACES.
              ENDIF.
            ENDIF.

            IF strlen( w_bdclm-mpar ) NE 0 .
              g_len = w_bdclm-mpar+0(2).
              IF g_len > 0 .
                SHIFT  w_bdclm-mpar BY 2 PLACES.
                g_msg2 = w_bdclm-mpar+0(g_len).
                SHIFT  w_bdclm-mpar BY g_len PLACES.
              ENDIF.
            ENDIF.

            IF strlen( w_bdclm-mpar ) NE 0 .
              g_len = w_bdclm-mpar+0(2).
              IF g_len > 0 .
                SHIFT  w_bdclm-mpar BY 2 PLACES.
                g_msg3 = w_bdclm-mpar+0(g_len).
                SHIFT  w_bdclm-mpar BY g_len PLACES.
              ENDIF.
            ENDIF.

            IF strlen( w_bdclm-mpar ) NE 0 .
              g_len = w_bdclm-mpar+0(2).
              IF g_len > 0 .
                SHIFT  w_bdclm-mpar BY 2 PLACES.
                g_msg4 = w_bdclm-mpar+0(g_len).
                SHIFT  w_bdclm-mpar BY g_len PLACES.
              ENDIF.
            ENDIF.

            CALL FUNCTION 'FORMAT_MESSAGE'
              EXPORTING
                id        = g_msg_id
                lang      = sy-langu
                no        = g_msg_nr
                v1        = g_msg1
                v2        = g_msg2
                v3        = g_msg3
                v4        = g_msg4
              IMPORTING
                msg       = g_msg
              EXCEPTIONS
                not_found = 1
                OTHERS    = 2.
            IF sy-subrc = 0.

              w_result_messages-mandant = sy-mandt.
              w_result_messages-langu = sy-langu.
              w_result_messages-msgtxt = g_msg.
              APPEND w_result_messages TO result_messages.
              CLEAR w_result_messages.

            ENDIF.
          ENDLOOP.

          g_index = 0.

          IF g_status1 <> c_f.
*--------collect the successful msgs

            LOOP AT t_session_log INTO w_session_log.
*  Begin of INRYA4

              IF ( w_session_log-logmessage+73(1)  = c_msgtyp
                   AND w_session_log-logmessage+74(2) = 'F5'
                   AND w_session_log-logmessage+94(3) = '312' )
                   OR w_session_log-logmessage+73(1)  = 'W'
                   OR w_session_log-logmessage+73(1)  = 'I'
                   OR w_session_log-logmessage+94(3)  = '349'
                   OR w_session_log-logmessage+94(3)  = '344'.
* End of INRYA4
                g_index = g_index + 1.

                w_bdclm = w_session_log.

                g_msg_id = w_bdclm-mid.
                g_msg_nr = w_bdclm-mnr.

                IF strlen( w_bdclm-mpar ) NE 0 .
                  g_len = w_bdclm-mpar+0(2).
                  IF g_len > 0 .
                    SHIFT  w_bdclm-mpar BY 2 PLACES.
                    g_msg1 = w_bdclm-mpar+0(g_len).
                    IF w_session_log-logmessage+73(1)  = c_msgtyp
                          AND w_session_log-logmessage+74(2)  = c_msgcls
                          AND w_session_log-logmessage+94(3)  = c_msgno.
                      g_docno = g_msg1.

                      IF g_docno IS NOT INITIAL.
                        return_status = 'Y'.
                      ENDIF.
                    ENDIF.
                    SHIFT  w_bdclm-mpar BY g_len PLACES.
                  ENDIF.
                ENDIF.

                IF strlen( w_bdclm-mpar ) NE 0 .
                  g_len = w_bdclm-mpar+0(2).
                  IF g_len > 0 .
                    SHIFT  w_bdclm-mpar BY 2 PLACES.
                    g_msg2 = w_bdclm-mpar+0(g_len).
                    SHIFT  w_bdclm-mpar BY g_len PLACES.
                  ENDIF.
                ENDIF.

                IF strlen( w_bdclm-mpar ) NE 0 .
                  g_len = w_bdclm-mpar+0(2).
                  IF g_len > 0 .
                    SHIFT  w_bdclm-mpar BY 2 PLACES.
                    g_msg3 = w_bdclm-mpar+0(g_len).
                    SHIFT  w_bdclm-mpar BY g_len PLACES.
                  ENDIF.
                ENDIF.

                IF strlen( w_bdclm-mpar ) NE 0 .
                  g_len = w_bdclm-mpar+0(2).
                  IF g_len > 0 .
                    SHIFT  w_bdclm-mpar BY 2 PLACES.
                    g_msg4 = w_bdclm-mpar+0(g_len).
                    SHIFT  w_bdclm-mpar BY g_len PLACES.
                  ENDIF.
                ENDIF.

                CALL FUNCTION 'FORMAT_MESSAGE'
                  EXPORTING
                    id        = g_msg_id
                    lang      = sy-langu
                    no        = g_msg_nr
                    v1        = g_msg1
                    v2        = g_msg2
                    v3        = g_msg3
                    v4        = g_msg4
                  IMPORTING
                    msg       = g_msg
                  EXCEPTIONS
                    not_found = 1
                    OTHERS    = 2.
                IF sy-subrc = 0.
                  w_result_messages-mandant = sy-mandt.
                  w_result_messages-langu = sy-langu.
                  w_result_messages-msgtxt = g_msg.
                  APPEND w_result_messages TO result_messages.
                  CLEAR w_result_messages.
                ENDIF.
                g_status1  = c_s.
              ENDIF.
            ENDLOOP.
          ENDIF.
          CLEAR g_status1.
        ENDIF.

*      DELETE DATASET g_filename.

* Sundry Invoice Text
        PERFORM update_sundry USING company_code
                                    posting_date
                                    long_text.

*Long Text at Line Items
        PERFORM update_longtext USING company_code
                                      posting_date.

        IF session_keep_option IS INITIAL.
          PERFORM delete_batch_input.
        ENDIF.
      ENDIF.       " INUJA5
*      ENDIF.       " INUJA5
    ENDIF.
    REFRESH t_session_log.
  ENDLOOP.

ENDFUNCTION.
