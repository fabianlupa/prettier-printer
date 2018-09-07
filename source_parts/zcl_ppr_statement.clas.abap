"! Statement
CLASS zcl_ppr_statement DEFINITION
  PUBLIC
  CREATE PUBLIC
  GLOBAL FRIENDS zif_ppr_formatting_rule.

  PUBLIC SECTION.
    INTERFACES:
      zif_ppr_source_container.
    METHODS:
      constructor IMPORTING io_scan_statement TYPE REF TO zcl_ppr_scan_statement,
      format_after_child_contexts RETURNING VALUE(rv_late) TYPE abap_bool,
      get_statement_text RETURNING VALUE(rv_text) TYPE string,
      get_dot_position RETURNING VALUE(rv_position) TYPE i,
      get_start_line RETURNING VALUE(rv_start_line) TYPE i,
      get_end_line RETURNING VALUE(rv_end_line) TYPE i,
      set_start_line IMPORTING iv_line TYPE i.
  PROTECTED SECTION.
    DATA:
      mo_scan_statement TYPE REF TO zcl_ppr_scan_statement,
      mt_source         TYPE stringtab,
      mv_start_line     TYPE i,
      mv_end_line       TYPE i.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_statement IMPLEMENTATION.
  METHOD constructor.
    mo_scan_statement = io_scan_statement.
    mv_start_line = io_scan_statement->get_first_line_number( ).
    mv_end_line = io_scan_statement->get_last_line_number( ).
    mt_source = io_scan_statement->get_source( ).
  ENDMETHOD.

  METHOD format_after_child_contexts.
    rv_late = abap_false.

    IF mt_source[ 1 ] CP '*ENDCLASS*' ##TODO.
      rv_late = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_statement_text.
    rv_text = mo_scan_statement->get_statement_text( ).
  ENDMETHOD.

  METHOD get_dot_position.
    rv_position = mo_scan_statement->get_dot_position( ).
  ENDMETHOD.

  METHOD get_end_line.
    rv_end_line = mv_end_Line.
  ENDMETHOD.

  METHOD get_start_line.
    rv_start_line = mv_start_line.
  ENDMETHOD.

  METHOD set_start_line.
    mv_start_line = iv_line.
    mv_end_line = mv_start_line + lines( mt_source ).
  ENDMETHOD.

  METHOD zif_ppr_source_container~get_source_code.
    rt_source = mt_source.
  ENDMETHOD.

  METHOD zif_ppr_source_container~get_start_line.
    rv_line = get_start_line( ).
  ENDMETHOD.
ENDCLASS.
