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
      remove_unnecessary_whitespace,
      format_after_child_contexts RETURNING VALUE(rv_late) TYPE abap_bool.
  PROTECTED SECTION.
    DATA:
      mo_scan_statement TYPE REF TO zcl_ppr_scan_statement,
      mt_source         TYPE stringtab.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_statement IMPLEMENTATION.
  METHOD constructor.
    mo_scan_statement = io_scan_statement.
    mt_source = io_scan_statement->get_source( ).
  ENDMETHOD.

  METHOD zif_ppr_formattable~format.
    remove_unnecessary_whitespace( ).
    rt_formatted = mt_source.
  ENDMETHOD.

  METHOD remove_unnecessary_whitespace.

  ENDMETHOD.

  METHOD format_after_child_contexts.
    rv_late = abap_false.

    IF mt_source[ 1 ] CP '*ENDCLASS*' ##TODO.
      rv_late = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
