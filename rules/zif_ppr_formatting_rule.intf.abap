"! Formatting rule
INTERFACE zif_ppr_formatting_rule PUBLIC.
  METHODS:
    get_settings_type RETURNING VALUE(rv_typename) TYPE abap_typename,
    apply_rule IMPORTING ir_code TYPE REF TO stringtab.
ENDINTERFACE.
