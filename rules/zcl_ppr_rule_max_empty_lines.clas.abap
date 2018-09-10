"! Maximum empty lines rule
CLASS zcl_ppr_rule_max_empty_lines DEFINITION
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



CLASS zcl_ppr_rule_max_empty_lines IMPLEMENTATION.
  METHOD apply_internal.
    DATA(lo_context) = CAST zcl_ppr_ctx_empty( io_target ).

    IF lo_context->get_empty_line_amount( ) > 3 ##TODO. " Settings
      lo_context->set_empty_line_amount( 3 ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_ppr_formatting_rule~get_settings_group_name.

  ENDMETHOD.

  METHOD zif_ppr_formatting_rule~get_formatting_relevancy.
    rt_relevancy = VALUE #(
       ( type      = zif_ppr_formatting_rule=>gc_relevancy_type-context_classname
         classname = 'ZCL_PPR_CTX_EMPTY' )
     ).
  ENDMETHOD.
ENDCLASS.
