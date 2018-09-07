"! Dummy context for empty lines
CLASS zcl_ppr_ctx_empty DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_ppr_context
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_parent TYPE REF TO zcl_ppr_context OPTIONAL,
      zif_ppr_source_container~get_line_count REDEFINITION,
      zif_ppr_source_container~get_source_code REDEFINITION,
      set_empty_line_amount IMPORTING iv_amount TYPE i,
      get_empty_line_amount RETURNING VALUE(rv_amount) TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mv_empty_lines.
ENDCLASS.



CLASS zcl_ppr_ctx_empty IMPLEMENTATION.
  METHOD get_empty_line_amount.
    rv_amount = mv_empty_lines.
  ENDMETHOD.

  METHOD set_empty_line_amount.
    mv_empty_lines = iv_amount.
  ENDMETHOD.

  METHOD zif_ppr_source_container~get_line_count.
    rv_line_count = mv_empty_lines.
  ENDMETHOD.

  METHOD zif_ppr_source_container~get_source_code.
    DO mv_empty_lines TIMES.
      APPEND INITIAL LINE TO rt_source.
    ENDDO.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( io_parent = io_parent ).
  ENDMETHOD.
ENDCLASS.
