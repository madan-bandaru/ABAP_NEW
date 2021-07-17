************************************************************************
*                         PROGRAM INFORMATION                          *
************************************************************************
* RIEF ID.......: FSSR980                                              *
* PROGRAM.......: /DS1/FI_N_BSPWF_REQ_TOP                              *
* TITLE.........  BSP Workflow Requests Report                         *
* OWNER.........: Vellaippan Thirunavukkarasu                          *
* AUTHOR........: Chaitanya Sudabatula(INCSU4)                         *
* DATE WRITTEN..: 10th Jan 2011                                        *
* R/3 RELEASE...: R2.3 - IR MAY 2011                                   *
* REV TRAC......: 46243                                                *
* TRANSPORTNR...: D94K9A38TI                                           *
* COPIED FROM...: N/A                                                  *
*----------------------------------------------------------------------*
* PROGRAM FUNCTION: Top include for the BSP workflow requests report   *
*----------------------------------------------------------------------*
* PROGRAM TYPE.. Report                                                *
* DEV. CLASS.... /DS1/FSS_01                                           *
* LOGICAL DB.... N/A                                                   *
************************************************************************

************************************************************************
*                         TYPE-POOLS                                   *
************************************************************************
TYPE-POOLS:slis,
           sdydo.

************************************************************************
*                         TYPES Declaration                            *
************************************************************************
TYPES:BEGIN OF x_roleid,
       agr_name TYPE agr_name,
      END OF x_roleid,

      BEGIN OF x_dataobj,
       object TYPE /ds1/mdm_object,
       description TYPE /ds1/mdm_object_descr,
      END OF x_dataobj,

      BEGIN OF x_wftemplates,
        object TYPE /ds1/mdm_object,
        wf_task TYPE sww_task,
      END OF x_wftemplates,

      BEGIN OF x_users,
        uname TYPE xubname,
      END OF x_users,

      BEGIN OF x_wi,
       wi_id TYPE sww_wiid,
       wi_stat TYPE sww_wistat,
       wi_cd TYPE sww_cd,
       wi_ct TYPE sww_ct,
       wi_aed TYPE sww_aed,
       wi_aagent TYPE sww_aagent,
       wi_rh_task TYPE sww_task,
      END OF x_wi,

      BEGIN OF x_subwi,
       wi_id TYPE sww_wiid,
       wi_stat TYPE sww_wistat,
       wi_cd TYPE sww_cd,
       wi_ct TYPE sww_ct,
       wi_aed TYPE sww_aed,
       wi_aagent TYPE sww_aagent,
       top_wi_id TYPE swfrtwiid,
       read_wi TYPE flag,
      END OF x_subwi,

      BEGIN OF x_wfcontdata,
       object TYPE /ds1/mdm_object,
       wi_id TYPE sww_wiid,
       wi_rh_task TYPE sww_task,
       note_id TYPE numc3,
       creation_date TYPE bu_crdat,
       creation_time TYPE bu_crtim,
       agent TYPE uname,
       text TYPE text255,
       bukrs TYPE bukrs,
       taskid TYPE sww_wiid,
       rejflag TYPE flag,
       retflag TYPE flag,
      END OF x_wfcontdata,

      BEGIN OF x_rejrept,
       wi_id TYPE sww_wiid,
       bukrs TYPE bukrs,
       land1 TYPE land1,
       obj_desc TYPE /ds1/mdm_object_description,
       agent TYPE uname,
       rejres TYPE text255,
      END OF x_rejrept,

      BEGIN OF x_clopnrep,
       wi_id TYPE sww_wiid,
       bukrs TYPE bukrs,
       land1 TYPE land1,
       obj_desc TYPE /ds1/mdm_object_description,
       wi_stat TYPE sww_wistat,
       agent TYPE uname,
       wi_cd TYPE sww_cd,
       wi_aed TYPE sww_aed,
       nodays TYPE int4,
       porf   TYPE char10,
      END OF x_clopnrep.



************************************************************************
*                     Global Data  Declaration                         *
************************************************************************
DATA:g_land1 TYPE land1,
     g_role  TYPE agr_name,
     g_dataobj TYPE /ds1/mdm_object,
     g_bukrs TYPE bukrs,
     g_count TYPE int4,
     g_status TYPE sww_wistat,
     c_alo_rej_status TYPE char2.

DATA:r_roleid TYPE RANGE OF agr_name,
     r_dataobj TYPE RANGE OF /ds1/mdm_object,
     r_wftemplates TYPE RANGE OF sww_task.

************************************************************************
*                 Internal tables  Declaration                         *
************************************************************************
DATA:t_wftemplates TYPE STANDARD TABLE OF x_wftemplates,
     t_users TYPE STANDARD TABLE OF x_users,
     t_dataobj TYPE STANDARD TABLE OF x_dataobj,
     t_wi TYPE STANDARD TABLE OF x_wi,
     t_subwi TYPE STANDARD TABLE OF x_subwi,
     t_allwi TYPE STANDARD TABLE OF x_subwi,
     lt_allwi TYPE STANDARD TABLE OF x_subwi,
     t_wfcontdata TYPE STANDARD TABLE OF x_wfcontdata,
     t_clopnrep TYPE STANDARD TABLE OF /ds1/fi_ms_fcat_clopnrep,
     t_rejrep TYPE STANDARD TABLE OF /ds1/fi_ms_fcat_rejrep,
     t_fieldcat_clopn TYPE slis_t_fieldcat_alv,            "Fieldcat int table
     t_fieldcat_rejrep TYPE slis_t_fieldcat_alv,            "Fieldcat int table
     t_events    TYPE slis_t_event,              "Hold Events
     w_wi TYPE x_wi,
     lw_allwi TYPE x_subwi,
     w_wftemplates TYPE x_wftemplates.



************************************************************************
*                   Global Constants Declaration                       *
************************************************************************
CONSTANTS:c_prog TYPE programm VALUE '/DS1/FI_CE_REPT980_BSPWF_REQ',
          c_buss_ind TYPE /ds1/buss_ind1 VALUE 'FSSR980',
          c_x TYPE flag VALUE 'X',
          c_i TYPE bapisign VALUE 'I',
          c_eq TYPE bapioption VALUE 'EQ',
          c_alo TYPE /ds1/mdm_object VALUE 'ALO',
          c_ast TYPE /ds1/mdm_object VALUE 'AST',
          c_bnk TYPE /ds1/mdm_object VALUE 'BNK',
          c_coc TYPE /ds1/mdm_object VALUE 'COC',
          c_gfr TYPE /ds1/mdm_object VALUE 'GFR',
          c_gla TYPE /ds1/mdm_object VALUE 'GLA',
          c_prc TYPE /ds1/mdm_object VALUE 'PRC',
          c_sce TYPE /ds1/mdm_object VALUE 'SCE',
          c_t_wf_history TYPE swfdname VALUE 'T_WF_HISTORY',
          c_req_notes TYPE swfdname VALUE 'REQ_NOTES',
          c_comp_code TYPE swfdname VALUE 'COMPANY_CODE',
          c_wf_bukrs TYPE swfdname VALUE 'BUKRS',
          c_w_coce TYPE swfdname VALUE 'W_COCE',
          c_ms_frce TYPE swfdname VALUE 'MS_FRCE',
          c_skb1 TYPE swfdname VALUE 'SKB1',
          c_g_bukrs_routing TYPE swfdname VALUE 'G_BUKRS_ROUTING',
          c_key TYPE swfdname VALUE 'KEY',
          c_pass      TYPE char6   VALUE 'Passed',
          c_fail      TYPE char6   VALUE 'Failed',
          c_clopnrep TYPE tabname VALUE '/DS1/FI_MS_FCAT_CLOPNREP',
          c_rejrep TYPE tabname VALUE '/DS1/FI_MS_FCAT_REJREP',
          c_99991231 TYPE datum VALUE '99991231',
          c_completed TYPE sww_wistat VALUE 'COMPLETED',
          c_wi_id TYPE slis_fieldname VALUE 'WI_ID',
          c_bukrs TYPE slis_fieldname VALUE 'BUKRS',
          c_land1 TYPE slis_fieldname VALUE 'LAND1',
          c_agent TYPE slis_fieldname VALUE 'AGENT',
          c_obj_desc TYPE slis_fieldname VALUE 'OBJ_DESC',
          c_rejres TYPE slis_fieldname VALUE 'REJRES',
          c_wi_stat TYPE slis_fieldname VALUE 'WI_STAT',
          c_wi_cd TYPE slis_fieldname VALUE 'WI_CD',
          c_wi_aed TYPE slis_fieldname VALUE 'WI_AED',
          c_nodays TYPE slis_fieldname VALUE 'NODAYS',
          c_porf TYPE slis_fieldname VALUE 'PORF',
          c_colon TYPE char1 VALUE ':',
          c_def_rejstatus TYPE char2 VALUE '55',
          c_dot TYPE char1 VALUE '.',
          c_slash TYPE char1 VALUE '/',
          c_hypen TYPE char1 VALUE '-'.




************************************************************************
*                         Selection Screen                             *
************************************************************************
SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE text-000.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:rb1 RADIOBUTTON GROUP r1 DEFAULT 'X'
                           USER-COMMAND us1.
SELECTION-SCREEN COMMENT 3(75) text-001.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:rb2 RADIOBUTTON GROUP r1.
SELECTION-SCREEN COMMENT 3(75) text-002.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN:END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-003.
PARAMETERS:p_stdate TYPE datum OBLIGATORY,
           p_endate TYPE datum OBLIGATORY.

SELECT-OPTIONS:s_role FOR g_role OBLIGATORY,
               s_datob FOR g_dataobj OBLIGATORY," MATCHCODE OBJECT /ds1/mdm_object,
               s_orgunt FOR g_land1 OBLIGATORY MATCHCODE OBJECT
                                                  h_t005_land,
               s_bukrs FOR g_bukrs OBLIGATORY,
               s_reqsta FOR g_status OBLIGATORY MODIF ID sc1.
SELECTION-SCREEN END OF BLOCK b2.

*SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-003.
*PARAMETERS:p_list RADIOBUTTON GROUP r2,
*           p_grid RADIOBUTTON GROUP r2.
*SELECTION-SCREEN END OF BLOCK b4.
