FUNCTION-POOL ZFSSI1454_FB01.           "MESSAGE-ID ..

* INCLUDE /DS1/LFSSI1454_FB01D...            " Local class definition
TYPES: BEGIN OF x_bukrs,
         bukrs TYPE bukrs,      " Company Code
       END   OF x_bukrs,

       BEGIN OF x_longtext,
         item TYPE numc3,
         text TYPE char120,
       END OF x_longtext,

BEGIN OF x_temse,
  temseid     TYPE rstsoname,        " TemSe object name
  credate     TYPE apq_crda,         " Queue creation date
  cretime     TYPE apq_crti,         " Queue creation time
  groupid     TYPE apq_grpn,
  qid         TYPE apq_quid,
  STATUS      type APQ_STAT,
END OF x_temse,

BEGIN OF x_lat_ses,
 mandant    TYPE apq_mandt,         " Client
 groupid    TYPE apq_grpn,          " Group ID
 qid        TYPE apq_quid,          " Queue ID
 qstate     TYPE apq_stat,          " status
 credate    TYPE apq_crda,          " Date creation
 cretime    TYPE apq_crti,          " time Creation
END OF x_lat_ses,

   BEGIN OF x_i_session_log  ,     "Plain log information in TemSe
      enterdate       LIKE btctle-enterdate,
      entertime       LIKE btctle-entertime,
      logmessage(352) TYPE c,
    END OF  x_i_session_log,

*-Internal table type
x_ty_balmt           TYPE STANDARD TABLE OF balmt,
x_ty_tabrec          TYPE STANDARD TABLE OF /ds1/fi_ms_sapgen,
x_ty_bukrs           TYPE STANDARD TABLE OF x_bukrs,
x_ty_bdcdata         TYPE STANDARD TABLE OF bdcdata.

*-Constants
CONSTANTS:
     c_x                     TYPE char1  VALUE 'X',          "Flag Set
     " Begin of MOD-001
     c_null_value               TYPE wrbtr  VALUE '0.00',
     c_clearing_debit_post_key  TYPE char2  VALUE '40', "Debit Post Key
     c_clearing_credit_post_key TYPE char2  VALUE '50', "Credit Post Key
     c_clearing_gl_account      TYPE char10 VALUE '0003490900',
     c_negative              TYPE char2  VALUE -1,          "Value '-1'
     c_max_items             TYPE char3  VALUE '849',  "Max line items
     c_zero                  TYPE char1  VALUE 0,           "Value '0'
     " End of MOD-001
     c_one                   TYPE char1  VALUE 1,           "Value '1'
     c_two                   TYPE char1  VALUE 2,           "Value '2'
     c_three                 TYPE char1  VALUE 3,           "Value '3'
     c_four                  TYPE char1  VALUE 4,           "Value '4'
     c_five                  TYPE char1  VALUE 5,           "Value '5'
     c_six                   TYPE char1  VALUE 6,           "Value '6'
     c_f                     TYPE char1  VALUE 'F',          "Value 'F'
     c_bukrs                 TYPE char5  VALUE 'BUKRS',   "Comapny Code
     c_f_bkpf_buk            TYPE char10 VALUE 'F_BKPF_BUK',
     c_folder TYPE char50 VALUE '/local/data/interface',
     c_folder1 TYPE char50 VALUE 'fssi1454/inbound',
     c_slash  TYPE char1  VALUE '/',             "Forward Slash                                               "Authority Object
     c_actvt                 TYPE char5  VALUE 'ACTVT',      "Activity
     c_01                    TYPE char2  VALUE '01'.      "Activity Code

*-Work Area
DATA: w_tabrec TYPE /ds1/fi_ms_sapgen,
      w_comp_codes TYPE x_bukrs,
      t_comp_codes TYPE STANDARD TABLE OF x_bukrs.

*-Variable
DATA: g_flag TYPE char1.

DATA:
   w_bgr00     TYPE    bgr00, "Batch Input Structure for Session Data
   w_bbkpf     TYPE    bbkpf, "Document Header for Accntng Document
   w_bbseg    TYPE     bbseg,
   t_bbseg    TYPE STANDARD TABLE OF bbseg INITIAL SIZE 0,
   w_sapgen   TYPE /ds1/fi_ms_sapgen.
DATA: g_tdname   TYPE tdobname,     "Text object number
      t_tline   TYPE TABLE OF tline,
      w_tline TYPE tline,
      g_docno TYPE char10,
      g_index TYPE numc3,
      w_thead   TYPE thead.
DATA: g_amount TYPE wrbtr,
      g_date TYPE sy-datum,
      g_uzeit TYPE syuzeit,
      g_tabix TYPE sy-tabix,  " MOD-001
      g_filename TYPE char200,
      g_status TYPE char1,
      g_jobnam TYPE btcjob VALUE 'ZLCN_FIN_524',
      g_jobcount TYPE btcjobcnt,
      g_jobnam1   TYPE btcjob,
      w_t001a TYPE t001a,
      w_result_messages TYPE balmt,
      w_t001 TYPE t001,
      t_fimsg TYPE STANDARD TABLE OF fimsg INITIAL SIZE 0,
      t_longtext TYPE STANDARD TABLE OF x_longtext,
      w_fimsg TYPE fimsg,
      t_joblist TYPE STANDARD TABLE OF tbtco INITIAL SIZE 0,
      w_joblist TYPE tbtco,
      g_joblogtbl      TYPE STANDARD TABLE OF tbtc5,
      t_lat_ses        TYPE STANDARD TABLE OF x_lat_ses ,
      t_temse          TYPE STANDARD TABLE OF x_temse ,
      w_temse         TYPE x_temse,
      w_bdclm         TYPE bdclm,
      w_longtext TYPE x_longtext,
      t_session_log   TYPE STANDARD TABLE OF  x_i_session_log ,
      t_session_log_tmp   TYPE STANDARD TABLE OF  x_i_session_log ,
      w_session_log TYPE x_i_session_log,
      w_lat_ses       TYPE x_lat_ses,
      g_temp     TYPE i    VALUE 1,
      g_status1   TYPE char1,
*      g_index    TYPE sytabix,
      g_msg_id   TYPE arbgb,
      g_msg_nr   TYPE msgnr,
      g_msg1     TYPE string,
      g_msg2     TYPE string,
      g_msg3     TYPE string,
      g_msg4     TYPE string,
      g_msg      TYPE string,
      g_len(2)   TYPE n,
      g_value TYPE char15,
      g_dat_fmt TYPE xudatfm,
      g_dcm_fmt TYPE xudcpfm,
      g_item TYPE numc3,
g_input_line TYPE int2,
g_doc_post   TYPE int2,
g_lin TYPE i,
*Begin of Defect ID : 276293-Parked and Posting the document, however FI document not
*displayed in SAPGEN Error log
g_ind type sy-index.
*End of Defect ID : 276293-Parked and Posting the document, however FI document not
*displayed in SAPGEN Error log

* Declaration of local Constants
CONSTANTS :
      c_auth       TYPE rstsauthch VALUE 'BATCH',
                                                " Authority check
      c_prom       TYPE rstsprom   VALUE 'I',
                                                 " Processing mode
      c_rectyp     TYPE rstsrectyp VALUE 'VNL----', "Type
      c_msgcls     TYPE char2      VALUE 'FP', " message class
      c_msgtyp     TYPE char1      VALUE 'S',  " message type
      c_msgno      TYPE char3      VALUE '001'," message number
      c_progress   TYPE char1      VALUE 'R',

      c_s          TYPE char1      VALUE 'S',
      c_e          TYPE char1      VALUE 'E',
      c_space      TYPE char1      VALUE ' ',
      c_msgid_00   TYPE char2      VALUE '00',
      c_357        TYPE char3      VALUE '357',
      c_00         TYPE char2      VALUE '00',
      c_batch(1) TYPE c VALUE  'B',           " Value B
      c_9999(4)  TYPE n VALUE  '9999'.        " Value 9999

DATA:w_BWITH    TYPE     BWITH,
     t_BWITH    TYPE STANDARD TABLE OF BWITH INITIAL SIZE 0.

Types:begin of ty_bseg_temp,
      newko type KUNNR.
types:end of   ty_bseg_temp.
Types:begin of ty_KNBW,
        KUNNR type kunnr,
        BUKRS TYPE bukrs,
        WITHT type WITHT,
        WT_WITHCD type WT_WITHCD,
        WT_AGENT TYPE WT_WTAGT.
types:end of ty_KNBW.
data:t_bseg_temp    TYPE STANDARD TABLE OF ty_bseg_temp,
     w_bseg_temp    TYPE  ty_bseg_temp,
     T_KNBW TYPE STANDARD TABLE OF ty_KNBW,
     w_KNBW    TYPE  KNBW.
CONSTANTS:lc_dots TYPE char04  VALUE '.,,.'," Used for translate
          c_max   TYPE char3  VALUE '400'.
DATA:     l_kunnr(10) TYPE  c.
