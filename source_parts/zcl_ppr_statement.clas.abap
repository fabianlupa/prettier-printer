"! Statement
CLASS zcl_ppr_statement DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_ppr_formattable.
    METHODS:
      constructor IMPORTING io_scan_statement TYPE REF TO zcl_ppr_scan_statement,
      remove_unnecessary_whitespace,
      format_after_child_contexts RETURNING VALUE(rv_late) TYPE abap_bool,
      get_statement_text RETURNING VALUE(rv_text) TYPE string.
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
    CONSTANTS: lc_pattern TYPE string VALUE `  +`. " Boost doesn't allow ` {2,} for some reason?

    DATA(lo_regex) = NEW cl_abap_regex( lc_pattern ).

    IF mo_scan_statement->get_first_line_number( ) <> mo_scan_statement->get_last_line_number( ).
      RETURN.
    ENDIF.

    LOOP AT mt_source ASSIGNING FIELD-SYMBOL(<lv_line>).
      FIND REGEX `[^ ]` IN <lv_line> MATCH OFFSET DATA(lv_offset).
      DATA(lo_matcher) = lo_regex->create_matcher( text = <lv_line> ).
      WHILE lo_matcher->find_next( ) = abap_true.
        lo_matcher->replace_found( ` ` ).
      ENDWHILE.

      <lv_line> = |{ repeat( val = ` ` occ = lv_offset ) }{ lo_matcher->text }|.

*      FIND REGEX `[^ ]` IN <lv_line> MATCH OFFSET DATA(lv_offset).
*      REPLACE ALL OCCURRENCES OF ` ` IN <lv_line> WITH ``.
*      <lv_line> = |{ repeat( val = ` ` occ = lv_offset ) }{ <lv_line> }|.
    ENDLOOP.
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
ENDCLASS.
