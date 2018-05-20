"! Remove whitespace rule
CLASS zcl_ppr_rule_whitespace DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_ppr_formatting_rule_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      zif_ppr_formatting_rule~get_settings_type REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_PPR_RULE_WHITESPACE IMPLEMENTATION.


  METHOD zif_ppr_formatting_rule~get_settings_type.

  ENDMETHOD.
ENDCLASS.
