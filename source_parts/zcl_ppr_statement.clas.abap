"! Statement
CLASS zcl_ppr_statement DEFINITION
  PUBLIC
  CREATE PUBLIC
  GLOBAL FRIENDS zif_ppr_formatting_rule.

  PUBLIC SECTION.
    INTERFACES:
      zif_ppr_formattable.
    METHODS:
      constructor IMPORTING io_scan_statement TYPE REF TO zcl_ppr_scan_statement,
      remove_unnecessary_whitespace,
      format_after_child_contexts RETURNING VALUE(rv_late) TYPE abap_bool,
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
    mt_source = io_scan_statement->get_source( ).
  ENDMETHOD.

  METHOD zif_ppr_formattable~format.
    DATA(lt_rules) = zcl_ppr_rule_factory=>get_rules_for_statement( me ).

    LOOP AT lt_rules INTO DATA(lo_rule).
      lo_rule->apply_rule( ig_settings = '' ir_code = REF #( mt_source ) io_target = me ).
    ENDLOOP.

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

  METHOD get_statement_text.
    rv_text = mo_scan_statement->get_statement_text( ).
  ENDMETHOD.

  METHOD get_dot_position.
    rv_position = mo_scan_statement->get_dot_position( ).
  ENDMETHOD.
ENDCLASS.
