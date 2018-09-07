"! Method definition statement
CLASS zcl_ppr_stmnt_methdef DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_ppr_statement
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      get_method_name RETURNING VALUE(rv_name) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_stmnt_methdef IMPLEMENTATION.
  METHOD get_method_name.
    rv_name = mo_scan_statement->get_token( 2 )->get_token_text( ).
  ENDMETHOD.
ENDCLASS.
