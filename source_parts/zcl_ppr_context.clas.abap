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
                    RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_statement.
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


  METHOD zif_ppr_formattable~format.
    DATA: lt_late_statements TYPE STANDARD TABLE OF REF TO zcl_ppr_statement.

    DATA(lt_context_rules) = zcl_ppr_rule_factory=>get_rules_for_context( me ).
    LOOP AT lt_context_rules INTO DATA(lo_rule).
      lo_rule->apply_rule( ig_settings = '' ir_code = VALUE #( ) io_target = me ).
    ENDLOOP.

    LOOP AT mt_statements INTO DATA(lo_statement).
      IF lo_statement->format_after_child_contexts( ) = abap_false.
        " Format statements
        APPEND LINES OF lo_statement->zif_ppr_formattable~format( io_configuration ) TO rt_formatted.
      ELSE.
        APPEND lo_statement TO lt_late_statements.
      ENDIF.
    ENDLOOP.

    " Format child contexts
    LOOP AT mt_children INTO DATA(lo_child).
      APPEND LINES OF lo_child->zif_ppr_formattable~format( io_configuration ) TO rt_formatted.
    ENDLOOP.

    " Format late statements
    LOOP AT lt_late_statements INTO DATA(lo_late_statement).
      APPEND LINES OF lo_late_statement->zif_ppr_formattable~format( io_configuration ) TO rt_formatted.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_statement.
    ro_statement = mt_statements[ iv_index ].
  ENDMETHOD.
ENDCLASS.
