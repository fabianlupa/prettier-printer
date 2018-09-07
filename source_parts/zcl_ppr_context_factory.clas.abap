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
                     RETURNING VALUE(rt_contexts) TYPE zcl_ppr_context=>gty_context_tab,
      get_relevant_statements IMPORTING io_structure         TYPE REF TO zcl_ppr_scan_structure
                                        it_structures        TYPE zcl_ppr_scan_result=>gty_structure_object_tab
                              RETURNING VALUE(rt_statements) TYPE zcl_ppr_scan_result=>gty_statement_object_tab,
      build_ordered_child_list IMPORTING it_contexts                  TYPE zcl_ppr_context=>gty_context_tab
                                         it_statements                TYPE zcl_ppr_context=>gty_statement_tab
                               RETURNING VALUE(rt_ordered_components) TYPE zcl_ppr_context=>gty_child_tab.
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
      ro_context = NEW #( it_children = VALUE #( FOR line IN lt_contexts ( line ) ) ).
    ELSE.
      ASSERT 1 = 2 ##TODO.
    ENDIF.
  ENDMETHOD.

  METHOD build_contexts.
    DATA: lt_statements     TYPE zcl_ppr_context=>gty_statement_tab,
          lo_new_context    TYPE REF TO zcl_ppr_context,
          lt_sub_structures TYPE STANDARD TABLE OF REF TO zcl_ppr_scan_structure.

    LOOP AT it_structures INTO DATA(lo_structure).
      LOOP AT get_relevant_statements( io_structure  = lo_structure
                                       it_structures = lo_structure->get_all_sub_structures( ) )
           INTO DATA(lo_scan_statement).
        APPEND zcl_ppr_statement_factory=>get_statement_from_scan( lo_scan_statement ) TO lt_statements.
      ENDLOOP.

      CASE lo_structure->get_structure_type( ).
        WHEN zcl_ppr_constants=>gc_scan_struc_types-class.
          IF lo_structure->has_parent_structure( ) AND
             lo_structure->get_parent_structure( )->get_structure_type( ) <>
             zcl_ppr_constants=>gc_scan_struc_types-class.

            DATA(lt_tokens) = lo_structure->get_statement( 1 )->get_tokens( ).
            IF lines( lt_tokens ) >= 3 AND to_upper( lt_tokens[ 3 ]->get_token_text( ) ) = 'IMPLEMENTATION'.
              lo_new_context = NEW zcl_ppr_ctx_classimp(
                io_parent         = io_parent
                io_scan_structure = lo_structure
              ).
              lt_sub_structures = lo_structure->get_sub_structures( ).
            ELSE.
              lo_new_context = NEW zcl_ppr_ctx_classdef(
                io_parent         = io_parent
                io_scan_structure = lo_structure
              ).
              lt_sub_structures = lo_structure->get_sub_structures( ).
            ENDIF.
          ENDIF.

        WHEN zcl_ppr_constants=>gc_scan_struc_types-alternation.
          " IF ... ENDIF, TRY ... ENDTRY
*          LOOP AT lo_structure->get_sub_structures( ) INTO DATA(lo_cond_sub_structure).
*            INSERT lo_cond_sub_structure->get_statements( ) INTO lt_statements INDEX lines( lt_statements ) - 1.
*          ENDLOOP.
          IF lo_structure->get_statement( 1 )->get_token( 1 )->get_token_text( ) = 'IF'.
            lo_new_context = NEW zcl_ppr_ctx_condition(
              io_parent         = io_parent
              io_scan_structure = lo_structure
            ).
          ELSE.
            ASSERT 1 = 2 ##TODO.
          ENDIF.
          lt_sub_structures = lo_structure->get_sub_structures( ).
        WHEN zcl_ppr_constants=>gc_scan_struc_types-condition.
          " These are parts of an IF statement block, including the ELSEIF conditions
          lo_new_context = NEW #(
            io_parent         = io_parent
            io_scan_structure = lo_structure
          ).
          lt_sub_structures = lo_structure->get_sub_structures( ).
      ENDCASE.

      IF lo_new_context IS NOT BOUND.
        " Fallback solution as no specific context class has been determined
        lo_new_context = NEW #(
          io_parent         = io_parent
          io_scan_structure = lo_structure
        ).
        lt_sub_structures = lo_structure->get_sub_structures( ).
      ENDIF.

      DATA(lt_sub_contexts) = build_contexts(
        it_structures = lt_sub_structures
        io_parent     = lo_new_context
      ).
      lo_new_context->set_children( build_ordered_child_list(
        it_contexts   = lt_sub_contexts
        it_statements = lt_statements
      ) ).

      APPEND lo_new_context TO rt_contexts.

      CLEAR: lt_statements, lt_sub_structures, lt_sub_contexts.
      FREE lo_new_context.
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
            ##TODO.
            DATA(lt_direct) = io_structure->get_sub_structures( ).
            IF io_structure->get_structure_type( ) = zcl_ppr_constants=>gc_scan_struc_types-condition AND
               lo_structure->get_structure_type( ) = zcl_ppr_constants=>gc_scan_struc_types-alternation AND
               line_exists( lt_direct[ table_line = lo_structure ] ).
              CONTINUE.
            ENDIF.
            DELETE lt_statements WHERE table_line = lo_statement.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    rt_statements = lt_statements.
  ENDMETHOD.

  METHOD build_ordered_child_list.
    TYPES: BEGIN OF lty_sorted,
             start_line TYPE i,
             component  TYPE REF TO zif_ppr_source_container,
           END OF lty_sorted.
    DATA: lt_sorted TYPE SORTED TABLE OF lty_sorted WITH UNIQUE KEY start_line.

    LOOP AT it_statements INTO DATA(lo_statement).
      ##TODO. " This insert fails with chained statements
      INSERT VALUE #(
        start_line = lo_statement->mo_scan_statement->get_first_line_number( )
        component  = lo_statement
      ) INTO TABLE lt_sorted.
    ENDLOOP.

    LOOP AT it_contexts INTO DATA(lo_child_context).
      IF lines( lo_child_context->get_statements( ) ) = 0.
        CONTINUE ##TODO.
      ENDIF.
      INSERT VALUE #(
        start_line = lo_child_context->get_statement( 1 )->mo_scan_statement->get_first_line_number( )
        component  = lo_child_context
      ) INTO TABLE lt_sorted.
    ENDLOOP.

    ##TODO. " Maybe this works?
    DATA(lv_last_line) = 1.
    LOOP AT lt_sorted ASSIGNING FIELD-SYMBOL(<ls_sorted2>).
      IF sy-tabix = 1.
        lv_last_line = <ls_sorted2>-start_line + <ls_sorted2>-component->get_line_count( ).
        CONTINUE.
      ENDIF.

      IF <ls_sorted2>-start_line > lv_last_line + 1.
        INSERT VALUE #(
          start_line = lv_last_line + 1
          component  = NEW zcl_ppr_ctx_empty( iv_line_amount = <ls_sorted2>-start_line - lv_last_line + 1 )
        ) INTO TABLE lt_sorted ##TODO. " Parent parameter is missing
      ENDIF.

      lv_last_line = <ls_sorted2>-start_line + <ls_sorted2>-component->get_line_count( ).
    ENDLOOP.

    LOOP AT lt_sorted ASSIGNING FIELD-SYMBOL(<ls_sorted>).
      APPEND <ls_sorted>-component TO rt_ordered_components.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
