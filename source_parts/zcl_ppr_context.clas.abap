"! Context
CLASS zcl_ppr_context DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_ppr_source_container.
    ALIASES:
      get_source_code FOR zif_ppr_source_container~get_source_code.
    TYPES:
      gty_context_tab          TYPE STANDARD TABLE OF REF TO zcl_ppr_context WITH DEFAULT KEY,
      gty_statement_tab        TYPE STANDARD TABLE OF REF TO zcl_ppr_statement WITH DEFAULT KEY,
      gty_source_component_tab TYPE STANDARD TABLE OF  REF TO zif_ppr_source_container WITH DEFAULT KEY.
    METHODS:
      constructor IMPORTING it_children       TYPE gty_context_tab OPTIONAL
                            io_parent         TYPE REF TO zcl_ppr_context OPTIONAL
                            it_statements     TYPE gty_statement_tab OPTIONAL
                            io_scan_structure TYPE REF TO zcl_ppr_scan_structure OPTIONAL,
      set_children IMPORTING it_children TYPE gty_context_tab,
      set_parent IMPORTING io_parent TYPE REF TO zcl_ppr_context,
      is_top_level_context RETURNING VALUE(rv_top_level) TYPE abap_bool,
      get_start_statement RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_statement,
      get_end_statement RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_statement,
      get_statements RETURNING VALUE(rt_statements) TYPE gty_statement_tab,
      set_statements IMPORTING it_statements TYPE gty_statement_tab,
      get_statement IMPORTING iv_index            TYPE i
                    RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_statement,
      get_components_in_order RETURNING VALUE(rt_components) TYPE gty_source_component_tab.
    DATA:
      mt_children TYPE gty_context_tab READ-ONLY,
      mo_parent   TYPE REF TO zcl_ppr_context READ-ONLY.
  PROTECTED SECTION.
    DATA:
      mo_scan_structure TYPE REF TO zcl_ppr_scan_structure,
      mt_statements     TYPE gty_statement_tab.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_context IMPLEMENTATION.
  METHOD constructor.
    mt_children = it_children.
    mo_parent = io_parent.
    mt_statements = it_statements.
    mo_scan_structure = io_scan_structure.
  ENDMETHOD.

  METHOD get_end_statement.
    ro_statement = mt_statements[ lines( mt_statements ) ].
  ENDMETHOD.

  METHOD get_start_statement.
*    IF mo_scan_structure->has_special_start_statement( ) = abap_true.
    ro_statement = mt_statements[ 1 ].
*    ELSE.
    ##TODO.
*    ENDIF.
  ENDMETHOD.

  METHOD get_statements.
    rt_statements = mt_statements.
  ENDMETHOD.

  METHOD set_statements.
    mt_statements = it_statements.
  ENDMETHOD.

  METHOD is_top_level_context.
    rv_top_level = boolc( mo_parent IS NOT BOUND ).
  ENDMETHOD.

  METHOD set_children.
    mt_children = it_children.
  ENDMETHOD.

  METHOD set_parent.
    mo_parent = io_parent.
  ENDMETHOD.

  METHOD get_statement.
    ro_statement = mt_statements[ iv_index ].
  ENDMETHOD.

  METHOD zif_ppr_source_container~get_source_code.
    LOOP AT get_components_in_order( ) INTO DATA(lo_component).
*      IF lo_component->get_start_line( ) > iv_start_line.
*        DO iv_start_line - lo_component->get_start_line( ) TIMES.
*          APPEND INITIAL LINE TO rt_source.
*          iv_start_line = iv_start_line + 1.
*        ENDDO.
*      ENDIF.
      DATA(lt_new_lines) = lo_component->get_source_code( ).
      APPEND LINES OF lt_new_lines TO rt_source.
*      iv_start_line = iv_start_line + lines(  lt_new_lines ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_components_in_order.
    TYPES: BEGIN OF lty_sorted,
             start_line TYPE i,
             component  TYPE REF TO zif_ppr_source_container,
           END OF lty_sorted.
    DATA: lt_sorted TYPE SORTED TABLE OF lty_sorted WITH UNIQUE KEY start_line.

    LOOP AT mt_statements INTO DATA(lo_statement).
      INSERT VALUE #(
        start_line = lo_statement->get_start_line( )
        component  = lo_statement
      ) INTO TABLE lt_sorted.
    ENDLOOP.

    LOOP AT mt_children INTO DATA(lo_child_context).
      IF lines( lo_child_context->get_statements( ) ) = 0.
        CONTINUE ##TODO.
      ENDIF.
      INSERT VALUE #(
        start_line = lo_child_context->get_statement( 1 )->get_start_line( )
        component  = lo_child_context
      ) INTO TABLE lt_sorted.
    ENDLOOP.

    LOOP AT lt_sorted ASSIGNING FIELD-SYMBOL(<ls_sorted>).
      APPEND <ls_sorted>-component TO rt_components.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_ppr_source_container~get_line_count.
    LOOP AT get_components_in_order( ) INTO DATA(li_component).
      rv_line_count = rv_line_count + li_component->get_line_count( ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
