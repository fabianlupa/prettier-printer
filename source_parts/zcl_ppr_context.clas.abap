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
      gty_child_tab     TYPE STANDARD TABLE OF REF TO zif_ppr_source_container WITH EMPTY KEY
                             WITH UNIQUE HASHED KEY unique COMPONENTS table_line,
      gty_statement_tab TYPE STANDARD TABLE OF REF TO zcl_ppr_statement WITH EMPTY KEY
                             WITH UNIQUE HASHED KEY unique COMPONENTS table_line,
      gty_context_tab   TYPE STANDARD TABLE OF REF TO zcl_ppr_context WITH EMPTY KEY
                             WITH UNIQUE HASHED KEY unique COMPONENTS table_line.
    METHODS:
      constructor IMPORTING it_children       TYPE gty_child_tab OPTIONAL
                            io_parent         TYPE REF TO zcl_ppr_context OPTIONAL
                            io_scan_structure TYPE REF TO zcl_ppr_scan_structure OPTIONAL,
      get_children RETURNING VALUE(rt_children) TYPE gty_child_tab,
      set_children IMPORTING it_children TYPE gty_child_tab,
      set_parent IMPORTING io_parent TYPE REF TO zcl_ppr_context,
      is_top_level_context RETURNING VALUE(rv_top_level) TYPE abap_bool,
      get_start_statement RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_statement,
      get_end_statement RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_statement,
      get_statements RETURNING VALUE(rt_statements) TYPE gty_statement_tab,
      get_statement IMPORTING iv_index            TYPE i
                    RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_statement.
    DATA:
      mo_parent   TYPE REF TO zcl_ppr_context READ-ONLY.
  PROTECTED SECTION.
    DATA:
      mt_children       TYPE gty_child_tab,
      mt_statements     TYPE gty_statement_tab,
      mt_contexts       TYPE gty_context_tab,
      mo_scan_structure TYPE REF TO zcl_ppr_scan_structure.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_context IMPLEMENTATION.
  METHOD constructor.
    mt_children = it_children.
    mo_parent = io_parent.
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

  METHOD is_top_level_context.
    rv_top_level = boolc( mo_parent IS NOT BOUND ).
  ENDMETHOD.

  METHOD set_children.
    mt_children = it_children.
    CLEAR: mt_statements, mt_contexts.

    LOOP AT mt_children INTO DATA(li_child).
      IF li_child IS INSTANCE OF zcl_ppr_statement.
        APPEND CAST #( li_child ) TO mt_statements.
      ELSEIF li_child IS INSTANCE OF zcl_ppr_context.
        APPEND CAST #( li_child ) TO mt_contexts.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_parent.
    mo_parent = io_parent.
  ENDMETHOD.

  METHOD get_statement.
    ro_statement = mt_statements[ iv_index ].
  ENDMETHOD.

  METHOD zif_ppr_source_container~get_source_code.
    LOOP AT mt_children INTO DATA(li_child).
      APPEND LINES OF li_child->get_source_code( ) TO rt_source.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_ppr_source_container~get_line_count.
    LOOP AT mt_children INTO DATA(li_child).
      rv_line_count = rv_line_count + li_child->get_line_count( ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_children.
    rt_children = mt_children.
  ENDMETHOD.
ENDCLASS.
