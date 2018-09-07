"! Intendation Rule
CLASS zcl_ppr_rule_intendation DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_ppr_formatting_rule_base
  ABSTRACT ##TODO
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



CLASS zcl_ppr_rule_intendation IMPLEMENTATION.
  METHOD apply_internal.

  ENDMETHOD.

  METHOD zif_ppr_formatting_rule~get_formatting_relevancy.
    rt_relevancy = VALUE #(
      ( type = zif_ppr_formatting_rule=>gc_relevancy_type-all_contexts )
    ).
  ENDMETHOD.

  METHOD zif_ppr_formatting_rule~get_settings_group_name.

  ENDMETHOD.
ENDCLASS.
