interface ZIF_FI_TAX_CALL_CONTROL
  public .


  methods TAX_CALL_CONTROL
    changing
      value(CH_EXIT) type CHAR1 optional .
endinterface.
