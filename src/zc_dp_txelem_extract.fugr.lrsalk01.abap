*----------------------------------------------------------------------*
*   INCLUDE LRSALK01                                                   *
*----------------------------------------------------------------------*
DATA: RSAL_SAVE_SUBRC LIKE SY-SUBRC,
      RSAL_S_LOGPARMS LIKE RSLOGPARMS.

* --------------------------------------------------------------------
*  Open application log
* --------------------------------------------------------------------
DEFINE LOG_OPEN.
  RSAL_S_LOGPARMS-FUNCTION   = &1.
  RSAL_S_LOGPARMS-LOGSYS     = &2.
  RSAL_S_LOGPARMS-INFOSOURCE = &3.
  RSAL_S_LOGPARMS-TYPE       = &4.
  CALL FUNCTION 'RSAL_LOG_OPEN'
       EXPORTING
            I_FUNCTION      = RSAL_S_LOGPARMS-FUNCTION
            I_LOGSYS        = RSAL_S_LOGPARMS-LOGSYS
            I_INFOSOURCE    = RSAL_S_LOGPARMS-INFOSOURCE
            I_TYPE          = RSAL_S_LOGPARMS-TYPE.
END-OF-DEFINITION.

* --------------------------------------------------------------------
*  Set logical system to log
* --------------------------------------------------------------------
DEFINE LOG_SET_LOGSYS.
  RSAL_S_LOGPARMS-LOGSYS     = &1.
  CALL FUNCTION 'RSAL_LOG_SET_LOGSYS'
       EXPORTING
            I_LOGSYS        = RSAL_S_LOGPARMS-LOGSYS.
END-OF-DEFINITION.

* --------------------------------------------------------------------
*  Reset logical system to log
* --------------------------------------------------------------------
DEFINE LOG_RESET_LOGSYS.
  CALL FUNCTION 'RSAL_LOG_RESET_LOGSYS'.
END-OF-DEFINITION.

* --------------------------------------------------------------------
*  Set InfoSource to log
* --------------------------------------------------------------------
DEFINE LOG_SET_ISOURCE.
  RSAL_S_LOGPARMS-INFOSOURCE = &1.
  RSAL_S_LOGPARMS-TYPE       = &2.
  CALL FUNCTION 'RSAL_LOG_SET_ISOURCE'
       EXPORTING
            I_OBJECT = RSAL_S_LOGPARMS-INFOSOURCE
            I_TYPE   = RSAL_S_LOGPARMS-TYPE.
END-OF-DEFINITION.

* --------------------------------------------------------------------
*  Reset InfoSource to log
* --------------------------------------------------------------------
DEFINE LOG_RESET_ISOURCE.
  CALL FUNCTION 'RSAL_LOG_RESET_ISOURCE'.
END-OF-DEFINITION.

* --------------------------------------------------------------------
*  Write log using specified message (using only first two message
*  values)
* --------------------------------------------------------------------
DEFINE LOG_WRITE.
  RSAL_SAVE_SUBRC = SY-SUBRC.
  RSAL_S_LOGPARMS-MSGTY = &1.
  RSAL_S_LOGPARMS-MSGID = &2.
  RSAL_S_LOGPARMS-MSGNO = &3.
  RSAL_S_LOGPARMS-MSGV1 = &4.
  RSAL_S_LOGPARMS-MSGV2 = &5.
  CALL FUNCTION 'RSAL_LOG_WRITE'
       EXPORTING
            I_MSGTY      = RSAL_S_LOGPARMS-MSGTY
            I_MSGID      = RSAL_S_LOGPARMS-MSGID
            I_MSGNO      = RSAL_S_LOGPARMS-MSGNO
            I_MSGV1      = RSAL_S_LOGPARMS-MSGV1
            I_MSGV2      = RSAL_S_LOGPARMS-MSGV2.
  SY-SUBRC = RSAL_SAVE_SUBRC.
END-OF-DEFINITION.

* --------------------------------------------------------------------
*  Write message with all message parameters
* --------------------------------------------------------------------
DEFINE LOG_WRITE_FULL.
  RSAL_SAVE_SUBRC = SY-SUBRC.
  RSAL_S_LOGPARMS-MSGTY = &1.
  RSAL_S_LOGPARMS-MSGID = &2.
  RSAL_S_LOGPARMS-MSGNO = &3.
  RSAL_S_LOGPARMS-MSGV1 = &4.
  RSAL_S_LOGPARMS-MSGV2 = &5.
  RSAL_S_LOGPARMS-MSGV3 = &6.
  RSAL_S_LOGPARMS-MSGV4 = &7.
  CALL FUNCTION 'RSAL_LOG_WRITE'
       EXPORTING
            I_MSGTY      = RSAL_S_LOGPARMS-MSGTY
            I_MSGID      = RSAL_S_LOGPARMS-MSGID
            I_MSGNO      = RSAL_S_LOGPARMS-MSGNO
            I_MSGV1      = RSAL_S_LOGPARMS-MSGV1
            I_MSGV2      = RSAL_S_LOGPARMS-MSGV2
            I_MSGV3      = RSAL_S_LOGPARMS-MSGV3
            I_MSGV4      = RSAL_S_LOGPARMS-MSGV4.
  SY-SUBRC = RSAL_SAVE_SUBRC.
END-OF-DEFINITION.

* --------------------------------------------------------------------
*  Write log using last error message from 'sy' variables
*  without closing log
* --------------------------------------------------------------------
DEFINE LOG_WRITE_ERROR.
  RSAL_SAVE_SUBRC = SY-SUBRC.
  CALL FUNCTION 'RSAL_LOG_WRITE'.
  SY-SUBRC = RSAL_SAVE_SUBRC.
END-OF-DEFINITION.

* --------------------------------------------------------------------
*  Write log using last error message from 'sy' variables
*  and close log
* --------------------------------------------------------------------
DEFINE LOG_WRITE_ERROR_AND_CLOSE.
  RSAL_SAVE_SUBRC = SY-SUBRC.
  CALL FUNCTION 'RSAL_LOG_WRITE'
       EXPORTING
            I_CLOSE_LOG  = 'X'.
  SY-SUBRC = RSAL_SAVE_SUBRC.
END-OF-DEFINITION.

* --------------------------------------------------------------------
*  Write log using last error message from 'sy' variables,
*  close log and send message
* --------------------------------------------------------------------
define log_write_error_close_message.
  rsal_save_subrc = sy-subrc.
  call function 'RSAL_LOG_WRITE'
       exporting
            i_close_log  = 'X'.
  message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 raising &1.
  sy-subrc = rsal_save_subrc.
end-of-definition.

* --------------------------------------------------------------------
*  Write internal log tables to database and close log
* --------------------------------------------------------------------
DEFINE LOG_CLOSE.
  RSAL_SAVE_SUBRC = SY-SUBRC.
  CALL FUNCTION 'RSAL_LOG_CLOSE'.
  SY-SUBRC = RSAL_SAVE_SUBRC.
END-OF-DEFINITION.

* --------------------------------------------------------------------
*  Rollback previous database operations write internal log
*  tables to database and close log
* --------------------------------------------------------------------
DEFINE LOG_CLOSE_AND_ROLLBACK.
  RSAL_SAVE_SUBRC = SY-SUBRC.
  ROLLBACK WORK.
  CALL FUNCTION 'RSAL_LOG_CLOSE'.
  SY-SUBRC = RSAL_SAVE_SUBRC.
END-OF-DEFINITION.
