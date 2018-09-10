"! Conditional statement
CLASS zcl_ppr_stmnt_condition DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_ppr_statement
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_scan_statement TYPE REF TO zcl_ppr_scan_statement.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_stmnt_condition IMPLEMENTATION.
  METHOD constructor.
    super->constructor( io_scan_statement ).
  ENDMETHOD.
ENDCLASS.
