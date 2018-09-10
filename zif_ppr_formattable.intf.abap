"! Object that can be pretty printed
INTERFACE zif_ppr_formattable PUBLIC.
  METHODS:
    format IMPORTING io_configuration    TYPE REF TO zcl_ppr_configuration
           RETURNING VALUE(rt_formatted) TYPE stringtab.
ENDINTERFACE.
