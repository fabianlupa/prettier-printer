"! Context hierarchy factory
CLASS zcl_ppr_context_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_context_hierarchy_by_scan IMPORTING io_result         TYPE REF TO zcl_ppr_scan_result
                                    RETURNING VALUE(ro_context) TYPE REF TO zcl_ppr_context.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      gty_context_tab TYPE STANDARD TABLE OF REF TO zcl_ppr_context WITH DEFAULT KEY.
    CLASS-METHODS:
      build_contexts IMPORTING it_structures      TYPE zcl_ppr_scan_result=>gty_structure_object_tab
                               io_parent          TYPE REF TO zcl_ppr_context OPTIONAL
                     RETURNING VALUE(rt_contexts) TYPE gty_context_tab,
      get_relevant_statements IMPORTING io_structure         TYPE REF TO zcl_ppr_scan_structure
                                        it_structures        TYPE zcl_ppr_scan_result=>gty_structure_object_tab
                              RETURNING VALUE(rt_statements) TYPE zcl_ppr_scan_result=>gty_statement_object_tab.
ENDCLASS.



CLASS zcl_ppr_context_factory IMPLEMENTATION.
  METHOD get_context_hierarchy_by_scan.
    DATA: lt_top_level_structures TYPE STANDARD TABLE OF REF TO zcl_ppr_scan_structure.

    LOOP AT io_result->mt_structures INTO DATA(lo_structure).
      IF lo_structure->has_parent_structure( ) = abap_true.
        CONTINUE.
      ENDIF.
      APPEND lo_structure TO lt_top_level_structures.
    ENDLOOP.

    DATA(lt_contexts) = build_contexts( lt_top_level_structures ).

    IF lines( lt_contexts ) = 1.
      ro_context = lt_contexts[ 1 ].
    ELSEIF lines(  lt_contexts ) > 1.
      ro_context = NEW #( it_children = lt_contexts ).
    ELSE.
      ASSERT 1 = 2 ##TODO.
    ENDIF.
  ENDMETHOD.

  METHOD build_contexts.
    DATA: lt_statements  TYPE STANDARD TABLE OF REF TO zcl_ppr_statement,
          lo_new_context TYPE REF TO zcl_ppr_context.

    LOOP AT it_structures INTO DATA(lo_structure).
      LOOP AT get_relevant_statements( io_structure  = lo_structure
                                       it_structures = lo_structure->get_all_sub_structures( ) )
           INTO DATA(lo_scan_statement).
        APPEND zcl_ppr_statement_factory=>get_statement_from_scan( lo_scan_statement ) TO lt_statements.
      ENDLOOP.

      CASE lo_structure->get_structure_type( ).
        WHEN zcl_ppr_constants=>gc_scan_struc_types-class.
          IF lo_structure->has_parent_structure( ) AND
             lo_structure->get_parent_structure( )->get_structure_type( ) <> zcl_ppr_constants=>gc_scan_struc_types-class.
            lo_new_context = NEW zcl_ppr_ctx_classdef(
              io_parent     = io_parent
              it_statements = lt_statements
            ).
          ELSE.
            lo_new_context = NEW #(
              io_parent     = io_parent
              it_statements = lt_statements
            ).
          ENDIF.
        WHEN OTHERS.
          lo_new_context = NEW #(
            io_parent     = io_parent
            it_statements = lt_statements
          ).
      ENDCASE.

      APPEND lo_new_context TO rt_contexts.

      DATA(lt_sub_contexts) = build_contexts(
        it_structures = lo_structure->get_sub_structures( )
        io_parent     = lo_new_context
      ).
      lo_new_context->set_children( lt_sub_contexts ).

      CLEAR: lt_statements.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_relevant_statements.
    " Statements are referred to from multiple structures at different hierarchy levels. To get
    " a single responsible structure find the most deeply nested structure that refers to the
    " statement.

    DATA(lt_statements) = io_structure->get_statements( ).
    LOOP AT lt_statements INTO DATA(lo_statement).
      LOOP AT it_structures INTO DATA(lo_structure) WHERE table_line <> io_structure.
        DATA(lt_statements_of_other) = lo_structure->get_statements( ).
        IF line_exists( lt_statements_of_other[ table_line->mv_id = lo_statement->mv_id ] ).
          " Is the other statement more deeply nested? Assume they are in a hierarchy.
          IF io_structure->is_descendant_of_mine( lo_structure ).
            DELETE lt_statements WHERE table_line = lo_statement.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    rt_statements = lt_statements.
  ENDMETHOD.
ENDCLASS.
