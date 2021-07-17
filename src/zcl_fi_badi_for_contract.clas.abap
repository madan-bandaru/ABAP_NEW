class ZCL_FI_BADI_FOR_CONTRACT definition
  public
  inheriting from CL_RECA_STORABLE_EXT
  final
  create public .

*"* public components of class ZCL_FI_BADI_FOR_CONTRACT
*"* do not include other source files here!!!
public section.

  methods IF_RECA_STORABLE_EXT~IS_MODIFIED
    redefinition .
protected section.
*"* protected components of class ZCL_FI_BADI_FOR_CONTRACT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_FI_BADI_FOR_CONTRACT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_FI_BADI_FOR_CONTRACT IMPLEMENTATION.


METHOD IF_RECA_STORABLE_EXT~IS_MODIFIED .
************************************************************************
* PROGRAM INFORMATION                                                  *
************************************************************************
* PROGRAM....... IF_EX_RECA_STORABLE_EXT~IS_MODIFIED (BAdI Method)     *
* TITLE......... Checks for Changed Data                               *
* AUTHOR........ Pasupuleti V Vardhan (INVPA1)                         *
* DATE WRITTEN.. 19 Aug 2005                                           *
* R/3 RELEASE... SAP Enterprise                                        *
* REv Trac/TRANSPORTNR...  894/D94K90916                               *
* COPIED FROM... N/A                                                   *
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:                                                    *
* Method IS_MODIFIED is used to check whether any data changes are     *
* happened in the subscreen 'Lease Audit' of Bussiness                 *
* Entity(Transaction : REBDBE). This will call the FM                  *
* '/DS1/C_CONRT_EVENT_XCHNG' which will return the flag whether any    *
* changes done or not                                                  *
*----------------------------------------------------------------------*
* PROGRAM TYPE.. Class Method                                          *
* DEV. CLASS.... /DS1/FSS_01                                           *
* LOGICAL DB.... <N/A>                                                 *
************************************************************************

************************************************************************
* CHANGE HISTORY                                                       *
************************************************************************
* DATE CHANGE... N/A                                                   *
* AUTHOR........ N/A                                                   *
* CHANGE DESCR.. N/A                                                   *
* R/3 RELEASE... N/A                                                   *
* TRANSPORTNR... N/A                                                   *
************************************************************************


*--Call the FM XCHNG to know whether any changes are done in subscreen
*--Lease audit or not
  CALL FUNCTION '/DS1/C_CONRT_EVENT_XCHNG'
    IMPORTING
      e_xchng = rf_modified.

ENDMETHOD.
ENDCLASS.
