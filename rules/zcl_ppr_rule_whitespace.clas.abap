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
    DATA(ls_settings) = CONV zppr_s_rule_whitespace( ig_settings ).
  ENDMETHOD.

  METHOD zif_ppr_formatting_rule~get_formatting_relevancy.
    rt_relevancy = VALUE #( ( type = zif_ppr_formatting_rule=>gc_relevancy_type-all_statements ) ).
  ENDMETHOD.
ENDCLASS.
