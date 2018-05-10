"! Scanned Statement
CLASS zcl_ppr_scan_statement DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING is_statement   TYPE sstmnt
                            iv_id          TYPE i
                            io_scan_result TYPE REF TO zcl_ppr_scan_result,
      get_tokens RETURNING VALUE(rt_tokens) TYPE zcl_ppr_scan_result=>gty_token_object_tab.
    DATA:
      ms_statement TYPE sstmnt READ-ONLY,
      mv_id        TYPE i READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
     mo_scan_result TYPE REF TO zcl_ppr_scan_result.
ENDCLASS.



CLASS zcl_ppr_scan_statement IMPLEMENTATION.
  METHOD constructor.
    ms_statement = is_statement.
    mv_id = iv_id.
    mo_scan_result = io_scan_result.
  ENDMETHOD.

  METHOD get_tokens.
    DO ms_statement-to - ms_statement-from TIMES.
      APPEND mo_scan_result->get_token_by_id( ms_statement-to + sy-index - 1 ) TO rt_tokens.
    ENDDO.
  ENDMETHOD.
ENDCLASS.
