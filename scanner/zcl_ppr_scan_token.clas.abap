"! Scanned Token
CLASS zcl_ppr_scan_token DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING is_token       TYPE stokesx
                            iv_id          TYPE i
                            io_scan_result TYPE REF TO zcl_ppr_scan_result,
      get_statement RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_scan_statement.
    DATA:
      ms_token TYPE stokesx READ-ONLY,
      mv_id    TYPE i READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mo_scan_result TYPE REF TO zcl_ppr_scan_result.
ENDCLASS.



CLASS zcl_ppr_scan_token IMPLEMENTATION.
  METHOD constructor.
    ms_token = is_token.
    mv_id = iv_id.
    mo_scan_result = mo_scan_result.
  ENDMETHOD.

  METHOD get_statement.
    ro_statement = mo_scan_result->get_statement_by_token( me ).
  ENDMETHOD.
ENDCLASS.
