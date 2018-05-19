"! Context hierarchy factory
CLASS zcl_ppr_hierarchy_factory DEFINITION
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
                     RETURNING VALUE(rt_contexts) TYPE gty_context_tab.
ENDCLASS.



CLASS zcl_ppr_hierarchy_factory IMPLEMENTATION.
  METHOD get_context_hierarchy_by_scan.
    DATA: lt_top_level_structures TYPE STANDARD TABLE OF REF TO zcl_ppr_scan_structure,
          lo_top                  TYPE REF TO zcl_ppr_context.

    LOOP AT io_result->mt_structures INTO DATA(lo_structure).
      IF lo_structure->has_parent_structure( ) = abap_true.
        CONTINUE.
      ENDIF.
      APPEND lo_structure TO lt_top_level_structures.
    ENDLOOP.

    DATA(lt_contexts) = build_contexts( lt_top_level_structures ).

    IF lines( lt_contexts ) = 1.
      lo_top = lt_contexts[ 1 ].
    ELSEIF lines(  lt_contexts ) > 1.
      lo_top = NEW #( it_children = lt_contexts ).
    ELSE.
      ASSERT 1 = 2 ##TODO.
    ENDIF.
  ENDMETHOD.

  METHOD build_contexts.
    DATA: lt_statements TYPE STANDARD TABLE OF REF TO zcl_ppr_statement.

    LOOP AT it_structures INTO DATA(lo_structure).
      LOOP AT lo_structure->get_statements( ) INTO DATA(lo_scan_statement).
        APPEND zcl_ppr_statement_factory=>get_statement_from_scan( lo_scan_statement ) TO lt_statements.
      ENDLOOP.

      APPEND NEW #(
        io_parent     = io_parent
        it_statements = lt_statements
      ) TO rt_contexts REFERENCE INTO DATA(lr_new).

      DATA(lt_sub_contexts) = build_contexts(
        it_structures = lo_structure->get_sub_structures( )
        io_parent     = lr_new->*
      ).
      lr_new->*->set_children( lt_sub_contexts ).

      CLEAR: lt_statements.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
