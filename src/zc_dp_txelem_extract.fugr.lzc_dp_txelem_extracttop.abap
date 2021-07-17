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
FUNCTION-POOL ZC_DP_TXELEM_EXTRACT.          "MESSAGE-ID ..

include lrsaxd01.

type-pools: abap.


types: begin of x_doc_ext,
          company_code          type bukrs,
          fi_document_no        type belnr_d,
          fi_document_year      type gjahr,
          fi_document_line_no   type buzei,
          liv_document_line_no  type rblgp,
          wo_settle_seq         type br_lfdnr,
          version               type /ds1/dpversion,
***Start of INSSHM
          gr_ref_doc   type lfbnr,
          gr_ref_doc_year type lfgja,
          gr_ref_doc_item type lfpos,
***End of INSSHM
          EXTRACT_LAST_CHANGE   TYPE timestamp,
*Start of Insertion Mod-001
          cpudt      type cpudt,"Day On Which Accounting
*                                 Document Was Entered
          cputm      type cputm," Time of Entry
          xblnr      type xblnr1," Reference Document Number
          awtyp      type awtyp, " Reference Transaction
          awkey      type awkey, " Reference Key
          monat      type monat, "Fiscal Period
*End of Insertion MOd-001
       end of x_doc_ext.

types: begin of x_doc_out,
          company_code            type bukrs,
          fi_document_no          type belnr_d,
          fi_document_year        type gjahr,
          fi_document_line_no     type buzei,
          liv_document_line_no    type rblgp,
          wo_settle_seq           type br_lfdnr,
          version                 type /ds1/dpversion,
          tax_element_nbr         type /ds1/dptxelem,
          taxable_state           type /txintf/tax_state,
          taxable_county          type /txintf/tax_county,
          taxable_city            type /txintf/ad_city1,
          authority_type          type /txintf/authority_type,
          amount_au               type /txintf/taxamt_acurr,
          basis_au                type /txintf/taxbas_acurr,
          authority_currency_code type /txintf/authority_currency,
       end of x_doc_out.

types: begin of x_bkpf,
          bukrs      type bukrs,
          belnr      type belnr_d,
          gjahr      type gjahr,
          cpudt      type cpudt,
          cputm      type cputm,
          xblnr      type xblnr1,
          awtyp      type awtyp,
          awkey      type awkey,
          monat      type monat,
       end of x_bkpf.

types: begin of x_ccode,
          bukrs type bukrs,
       end of x_ccode.


data: g_dpext        type cursor,
      g_initial_call type char1,
      g_s_if         type srsc_s_if_simple,
      g_packagesize  type srsc_s_if_simple-maxsize.

data: r_bukrs type range of bukrs,
      r_cpudt type range of cpudt,
      r_monat type range of monat,
      r_belnr type range of belnr_d,
      r_gjahr type range of gjahr.

data: t_bkpf       type table of x_bkpf,
      t_ccode      type table of x_ccode,
      t_ctl        type table of /ds1/seti_ctl,
      t_ctl_temp   type table of /ds1/seti_ctl,
      t_doc_ext    type table of x_doc_ext,
      t_doc_out    type table of x_doc_out,
      t_data       type table of /ds1/seti_tax_elem_ext_st,
      t_dfies      type standard table of dfies,
      t_where      type table of edpline..

data: w_ccode      type x_ccode,
      w_bkpf       type x_bkpf,
*      w_stamp      LIKE LINE OF r_stamp,
      w_ctl        type /ds1/seti_ctl,
      w_doc_ext    type x_doc_ext,
      w_doc_out    type x_doc_out,
      w_data       type /ds1/seti_tax_elem_ext_st,
      w_where      type edpline,
      w_select     type srsc_s_select,
      w_dfies      type dfies.

data: g_structdescr    type ref to cl_abap_structdescr,
      g_tabledescr     type ref to cl_abap_tabledescr,
      g_datadescr      type ref to cl_abap_datadescr,
      t_components     type abap_component_tab,
      w_component      type line of abap_component_tab,
      g_wa             type ref to data,
      g_tab            type ref to data,
      g_count          type i.

field-symbols: <fs_wa1>  type any,
               <fs_wa2>  type any,
               <fs_wa3>  type any,
               <fs_wa4>  type any,
               <fs_wa5>  type any,
               <fs_wa6>  type any,
               <fs_wa7>  type any,
               <fs_wa8>  type any,
               <fs_wa9>  type any,
               <fs_wa10> type any.

field-symbols: <fs_tab1>  type  standard table,
               <fs_tab2>  type  standard table,
               <fs_tab3>  type  standard table,
               <fs_tab4>  type  standard table,
               <fs_tab5>  type  standard table,
               <fs_tab6>  type  standard table,
               <fs_tab7>  type  standard table,
               <fs_tab8>  type  standard table,
               <fs_tab9>  type  standard table,
               <fs_tab10> type  standard table.


constants: c_datasource type roosourcer value 'ZDSFS_MIT_DIRECTPAY_REC'.
