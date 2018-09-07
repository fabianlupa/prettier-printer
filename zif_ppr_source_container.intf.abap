"! Object that contains source code
INTERFACE zif_ppr_source_container PUBLIC.
  METHODS:
    "! Get the contained source code as a string table
    get_source_code RETURNING VALUE(rt_source) TYPE stringtab,
    "! Get the amount of lines contained by this source container (including whitespace)
    get_line_count RETURNING VALUE(rv_line_count) TYPE i.
ENDINTERFACE.
