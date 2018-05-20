"! Formatting rule base class
CLASS zcl_ppr_formatting_rule_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_ppr_formatting_rule ABSTRACT METHODS get_settings_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_formatting_rule_base IMPLEMENTATION.
  METHOD zif_ppr_formatting_rule~apply_rule.

  ENDMETHOD.
ENDCLASS.
