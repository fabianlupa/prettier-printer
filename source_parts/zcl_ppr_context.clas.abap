"! Context
CLASS zcl_ppr_context DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_ppr_formattable.
    ALIASES:
      format FOR zif_ppr_formattable~format.
    TYPES:
      gty_context_tab   TYPE STANDARD TABLE OF REF TO zcl_ppr_context WITH DEFAULT KEY,
      gty_statement_tab TYPE STANDARD TABLE OF REF TO zcl_ppr_statement WITH DEFAULT KEY.
    METHODS:
      constructor IMPORTING it_children   TYPE gty_context_tab OPTIONAL
                            io_parent     TYPE REF TO zcl_ppr_context OPTIONAL
                            it_statements TYPE gty_statement_tab OPTIONAL,
      set_children IMPORTING it_children TYPE gty_context_tab,
      set_parent IMPORTING io_parent TYPE REF TO zcl_ppr_context,
      is_top_level_context RETURNING VALUE(rv_top_level) TYPE abap_bool,
      get_start_statement RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_statement,
      get_end_statement RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_statement,
      get_statements RETURNING VALUE(rt_statements) TYPE gty_statement_tab.
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
  ENDMETHOD.

  METHOD is_top_level_context.
    rv_top_level = boolc( mo_parent IS NOT BOUND ).
  ENDMETHOD.

  METHOD zif_ppr_formattable~format.
    LOOP AT mt_children INTO DATA(lo_child).
      APPEND LINES OF lo_child->zif_ppr_formattable~format( io_configuration ) TO rt_formatted.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_end_statement.

  ENDMETHOD.

  METHOD get_start_statement.

  ENDMETHOD.

  METHOD set_children.
    mt_children = it_children.
  ENDMETHOD.

  METHOD set_parent.
    mo_parent = io_parent.
  ENDMETHOD.

  METHOD get_statements.
    rt_statements = mt_statements.
  ENDMETHOD.
ENDCLASS.
