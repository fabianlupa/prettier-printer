"! Class definition context
CLASS zcl_ppr_ctx_classdef DEFINITION
  PUBLIC
  INHERITING FROM zcl_ppr_context
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      zif_ppr_formattable~format REDEFINITION,
      sort_sections_by_visibility.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_ctx_classdef IMPLEMENTATION.
  METHOD zif_ppr_formattable~format.
    sort_sections_by_visibility( ).
    rt_formatted = super->format( io_configuration ).
  ENDMETHOD.

  METHOD sort_sections_by_visibility.
    DATA: lt_sorted TYPE gty_context_tab.

    LOOP AT mt_children INTO DATA(lo_child).
*      IF lo_child->get_start_statement( )->
      INSERT lo_child INTO lt_sorted INDEX SWITCH #(
        lo_child->get_start_statement( )->get_statement_text( )
        WHEN 'PUBLIC SECTION'    THEN 1
        WHEN 'PROTECTED SECTION' THEN 2
        WHEN 'PRIVATE SECTION'   THEN 3
        ELSE lines( lt_sorted ) + 1
      ).
    ENDLOOP.

    mt_children = lt_sorted.
  ENDMETHOD.
ENDCLASS.
