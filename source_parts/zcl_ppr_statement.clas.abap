"! Statement
CLASS zcl_ppr_statement DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_ppr_formattable.
    METHODS:
      constructor IMPORTING io_scan_statement TYPE REF TO zcl_ppr_scan_statement,
      remove_unnecessary_whitespace.
  PROTECTED SECTION.
    DATA:
      mo_scan_statement TYPE REF TO zcl_ppr_scan_statement,
      mt_source         TYPE stringtab.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_statement IMPLEMENTATION.
  METHOD constructor.
    mo_scan_statement = io_scan_statement.
  ENDMETHOD.

  METHOD zif_ppr_formattable~format.
    remove_unnecessary_whitespace( ).
  ENDMETHOD.

  METHOD remove_unnecessary_whitespace.

  ENDMETHOD.
ENDCLASS.
