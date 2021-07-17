class ZCL_FI_TAX_CALL_CONTROL definition
  public
  final
  create public .

public section.

  interfaces ZIF_FI_TAX_CALL_CONTROL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FI_TAX_CALL_CONTROL IMPLEMENTATION.


  METHOD ZIF_FI_TAX_CALL_CONTROL~TAX_CALL_CONTROL.

*--Note : Free the Memory in the ZCL_SD_SERVICE_ENTRY_SHEET Class
*--Method - ZIF_SD_SERVICE_ENTRY_SHEET~SERVICE_ENTRY_ITEM_SPLIT
*--So that it work fine for the Bulk Tcode also

    DATA: l_total_count TYPE int4,
          l_count       TYPE int4.

*--To check the Total count the Enhancement can trigger
*--The count will be set in the ZCL_SD_SERVICE_ENTRY_SHEET Class
*--Method - ZIF_SD_SERVICE_ENTRY_SHEET~SERVICE_ENTRY_ITEM_SPLIT

    IMPORT l_total_count TO l_total_count FROM MEMORY ID 'TCNT'.
    IF l_total_count IS NOT INITIAL.
*--To check the number of times the Enhancement triggered
      IMPORT l_count TO l_count FROM MEMORY ID 'CCNT'.
      l_count = l_count + 1.
      EXPORT l_count FROM l_count TO MEMORY ID 'CCNT'.

*--If the number of count is GT then actual count, exit
      IF l_count GT l_total_count.
        ch_exit = 'Y'.
      ENDIF.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
