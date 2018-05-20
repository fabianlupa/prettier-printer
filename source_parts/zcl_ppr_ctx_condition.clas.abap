"! Condition context
CLASS zcl_ppr_ctx_condition DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_ppr_context
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      get_main_condition_statement RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_stmnt_condition,
      get_other_condition_statements RETURNING VALUE(rt_statements) TYPE gty_statement_tab,
      get_path_count RETURNING VALUE(rv_paths) TYPE i,
      get_path_context IMPORTING iv_index          TYPE i
                       RETURNING VALUE(ro_context) TYPE REF TO zcl_ppr_context,
      zif_ppr_formattable~format REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_ctx_condition IMPLEMENTATION.
  METHOD get_main_condition_statement.
    ro_statement = CAST #( get_start_statement( ) ).
  ENDMETHOD.

  METHOD get_other_condition_statements.
    " Other ELSEIF conditions are the first statement in the child contexts, other than the first one
    LOOP AT mt_children INTO DATA(lo_child) FROM 2.
      APPEND lo_child->get_statement( 1 ) TO rt_statements.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_path_context.
    ro_context = mt_children[ iv_index ].
  ENDMETHOD.

  METHOD get_path_count.
    rv_paths = lines( mt_children ).
  ENDMETHOD.

  METHOD zif_ppr_formattable~format.
  ENDMETHOD.
ENDCLASS.
