*----------------------------------------------------------------------*
*   INCLUDE LRSAXD01                                                   *
*----------------------------------------------------------------------*

* Interface parameters to Service API
TYPE-POOLS: sbiwa, rsap, srsc.

* Macros for message logging
INCLUDE lrsalk01.

* Global buffer for parameters passed by SAPI initialization call
DATA: g_s_interface TYPE sbiwa_s_interface.

* Global buffer for requested fields
DATA: g_t_fields TYPE sbiwa_t_fields.

* Global buffer for selection criteria
DATA: g_t_select TYPE sbiwa_t_select.

* Global buffer for selection language
DATA: g_t_langu  TYPE sbiwa_t_langu.

* Global buffer for time interval
DATA: g_s_timeint TYPE sbiwa_s_timeint.

* Global parameters
DATA: g_flag_interface_initialized,
      g_counter_datapakid TYPE sbiwa_s_interface-datapakid,
      g_cursor TYPE cursor.
