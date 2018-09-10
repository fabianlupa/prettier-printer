CLASS zcl_ppr_stmnt_typedef DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_ppr_statement
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      get_identifier RETURNING VALUE(rv_identifier) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_stmnt_typedef IMPLEMENTATION.
  METHOD get_identifier.
    rv_identifier = mo_scan_statement->get_token( 2 )->get_token_text( ).
  ENDMETHOD.
ENDCLASS.
