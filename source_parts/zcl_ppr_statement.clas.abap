"! Statement
CLASS zcl_ppr_statement DEFINITION
  PUBLIC
  CREATE PUBLIC
  GLOBAL FRIENDS zif_ppr_formatting_rule zcl_ppr_context_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_ppr_source_container.
    METHODS:
      constructor IMPORTING io_scan_statement TYPE REF TO zcl_ppr_scan_statement,
      get_statement_text RETURNING VALUE(rv_text) TYPE string,
      get_dot_position RETURNING VALUE(rv_position) TYPE i.
  PROTECTED SECTION.
    DATA:
      mo_scan_statement TYPE REF TO zcl_ppr_scan_statement,
      mt_source         TYPE stringtab.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_statement IMPLEMENTATION.
  METHOD constructor.
    mo_scan_statement = io_scan_statement.
    IF mo_scan_statement IS NOT INITIAL.
      mt_source = io_scan_statement->get_source( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_statement_text.
    rv_text = mo_scan_statement->get_statement_text( ).
  ENDMETHOD.

  METHOD get_dot_position.
    rv_position = mo_scan_statement->get_dot_position( ).
  ENDMETHOD.

  METHOD zif_ppr_source_container~get_source_code.
    rt_source = mt_source.
  ENDMETHOD.

  METHOD zif_ppr_source_container~get_line_count.
    rv_line_count = lines( mt_source ).
  ENDMETHOD.
ENDCLASS.
