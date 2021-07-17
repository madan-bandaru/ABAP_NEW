class ZCL_FI_SETI_FSS1457 definition
  public
  final
  create public .

public section.

  interfaces IF_EX_METH_FOBU_CONNECTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FI_SETI_FSS1457 IMPLEMENTATION.


  METHOD IF_EX_METH_FOBU_CONNECTOR~GET.
************************************************************************
*                         PROGRAM INFORMATION                          *
************************************************************************
* RIEF ID.......: FSSE1457_CR55000                                     *
* PROGRAM.......: /DS1/CL_FI_SETI_FSS1457                              *
* TITLE.........: Custom Class to add method GET_SBXTAXCODE in         *
*                 Special functions                                    *
* AUTHOR........: Sunil Kraplani(INSKTN)                               *
* DATE WRITTEN..: 08-Apr-2016                                          *
* REV TRAC......: 87218                                                *
* TRANSPORTNR...: D94K9A6ZXV                                           *
*----------------------------------------------------------------------*
* Class FUNCTION:
*          GET method to add method GET_SBXTAXCODE in Special functions
************************************************************************
    DATA: l_operand TYPE sfbeoprnd.

    CASE im_key.
      WHEN space.
        l_operand-tech_name = '_SPECIAL_FUNCS'.
        l_operand-descriptn = 'Special functions'(001).
        APPEND l_operand TO ch_operands.
      WHEN '_SPECIAL_FUNCS'.
        l_operand-tech_name = 'GET_SABRIXTXCODE'.
        l_operand-class = '/DS1/CL_FI_FSSE1457_SBXTAXCODE'.
        l_operand-method = 'GET_SABRIXTXCODE'.
        APPEND l_operand TO ch_operands.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
