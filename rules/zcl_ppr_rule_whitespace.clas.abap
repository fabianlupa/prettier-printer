"! Remove whitespace rule
CLASS zcl_ppr_rule_whitespace DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_ppr_formatting_rule_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      zif_ppr_formatting_rule~get_settings_group_name REDEFINITION,
      zif_ppr_formatting_rule~get_formatting_relevancy REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      apply_internal REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_rule_whitespace IMPLEMENTATION.
  METHOD zif_ppr_formatting_rule~get_settings_group_name.
    rv_name = 'WHITESPACE'.
  ENDMETHOD.

  METHOD apply_internal.
    CONSTANTS: lc_pattern TYPE string VALUE `  +`,
               lc_last    TYPE string VALUE `^.* \.$`.

    RETURN.

    DATA(lo_statement) = CAST zcl_ppr_statement( io_target ).


*    DATA(ls_settings) = CONV zppr_s_rule_whitespace( ig_settings ).
    IF lo_statement->mo_scan_statement->get_first_line_number( ) <>
       lo_statement->mo_scan_statement->get_last_line_number( ).
      " Multiple lines not supported yet
      RETURN.
    ENDIF.

    LOOP AT ir_code->* ASSIGNING FIELD-SYMBOL(<lv_line>).
*      FIND REGEX `[^ ]` IN <lv_line> MATCH OFFSET DATA(lv_offset).
*      DATA(lo_matcher) = lo_regex->create_matcher( text = <lv_line> ).
*      WHILE lo_matcher->find_next( ) = abap_true.
*        lo_matcher->replace_found( ` ` ).
*      ENDWHILE.
      DATA(lv_dot_position) = lo_statement->get_dot_position( ).
      DATA(lv_last_char_before_dot) = lv_dot_position - 1.
      WHILE lv_last_char_before_dot >= 0.
        IF <lv_line>+lv_last_char_before_dot(1) <> space.
          EXIT.
        ENDIF.
      ENDWHILE.
      IF lv_last_char_before_dot > 0.
        REPLACE SECTION OFFSET lv_last_char_before_dot LENGTH lv_dot_position - lv_last_char_before_dot
          OF <lv_line> WITH ''.
      ENDIF.

*      <lv_line>
*      <lv_line> = |{ repeat( val = ` ` occ = lv_offset ) }{ lo_matcher->text }|.

      FIND REGEX `[^ ]` IN <lv_line> MATCH OFFSET DATA(lv_offset).
      REPLACE ALL OCCURRENCES OF REGEX lc_pattern IN <lv_line> WITH ` `.
*      FIND FIRST OCCURRENCE OF REGEX lc_last IN <lv_line> SUBMATCHES DATA(lt_matches).
*      REPLACE SECTION OFFSET lv_offset2 LENGTH lv_length OF <lv_line> WITH '.'.
*      REPLACE ALL OCCURRENCES OF REGEX lc_last IN <lv_line> WITH ``.
      <lv_line> = |{ repeat( val = ` ` occ = lv_offset ) }{ <lv_line> }|.

*      BREAK-POINT.

      EXIT ##TODO.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_ppr_formatting_rule~get_formatting_relevancy.
    rt_relevancy = VALUE #( ( type = zif_ppr_formatting_rule=>gc_relevancy_type-all_statements ) ).
  ENDMETHOD.
ENDCLASS.
