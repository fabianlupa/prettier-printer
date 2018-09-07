"! Reorder class definitions rules
CLASS zcl_ppr_rule_reorder_cls_defs DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_ppr_formatting_rule_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      zif_ppr_formatting_rule~get_formatting_relevancy REDEFINITION,
      zif_ppr_formatting_rule~get_settings_group_name REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      apply_internal REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_rule_reorder_cls_defs IMPLEMENTATION.
  METHOD apply_internal.
    TYPES: BEGIN OF lty_definition,
             identifier TYPE string,
             statement  TYPE REF TO zcl_ppr_statement,
           END OF lty_definition,
           lty_definition_tab TYPE SORTED TABLE OF lty_definition WITH UNIQUE KEY identifier.
    DATA: BEGIN OF ls_statements,
            types         TYPE lty_definition_tab,
            constants     TYPE lty_definition_tab,
            events        TYPE lty_definition_tab,
            class_methods TYPE lty_definition_tab,
            methods       TYPE lty_definition_tab,
            class_data    TYPE lty_definition_tab,
            data          TYPE lty_definition_tab,
          END OF ls_statements,
          lt_ordered_statements TYPE zcl_ppr_context=>gty_statement_tab.
    FIELD-SYMBOLS: <ls_definition> TYPE lty_definition.

*    BREAK-POINT.

    DATA(lo_context) = CAST zcl_ppr_ctx_classdef( io_target ).

    DATA(lo_section) = lo_context->get_section_by_type( zcl_ppr_ctx_classdef=>gc_section_types-public ).
    LOOP AT lo_section->get_statements( ) INTO DATA(lo_statement).
      IF sy-tabix = 1.
        " First statement is the SECTION statement itself
        APPEND lo_statement TO lt_ordered_statements.
        CONTINUE.
      ENDIF.
      CASE TYPE OF lo_statement.
        WHEN TYPE zcl_ppr_stmnt_typedef INTO DATA(lo_typedef).
          INSERT VALUE #(
            identifier = lo_typedef->get_identifier( )
            statement  = lo_typedef
          ) INTO TABLE ls_statements-types.
        WHEN TYPE zcl_ppr_stmnt_methdef INTO DATA(lo_methdef).
          INSERT VALUE #(
            identifier = lo_methdef->get_method_name( )
            statement  = lo_methdef
          ) INTO TABLE ls_statements-methods.
      ENDCASE.
    ENDLOOP.

    LOOP AT ls_statements-types ASSIGNING <ls_definition>.
      APPEND <ls_definition>-statement TO lt_ordered_statements.
    ENDLOOP.
    UNASSIGN <ls_definition>.

    LOOP AT ls_statements-methods ASSIGNING <ls_definition>.
      APPEND <ls_definition>-statement TO lt_ordered_statements.
    ENDLOOP.
    UNASSIGN <ls_definition>.

*    lo_section->set_statements( lt_ordered_statements ).
    lo_section->set_ordered_components( lt_ordered_statements ).
  ENDMETHOD.

  METHOD zif_ppr_formatting_rule~get_settings_group_name.

  ENDMETHOD.

  METHOD zif_ppr_formatting_rule~get_formatting_relevancy.
    rt_relevancy = VALUE #(
      ( type      = zif_ppr_formatting_rule=>gc_relevancy_type-context_classname
        classname = 'ZCL_PPR_CTX_CLASSDEF' )
    ).
  ENDMETHOD.
ENDCLASS.
