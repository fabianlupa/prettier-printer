"! Rule factory
CLASS zcl_ppr_rule_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      gty_rule_tab TYPE STANDARD TABLE OF REF TO zif_ppr_formatting_rule WITH DEFAULT KEY.
    CLASS-METHODS:
      get_rules_for_context IMPORTING io_context      TYPE REF TO zcl_ppr_context
                            RETURNING VALUE(rt_rules) TYPE gty_rule_tab,
      get_rules_for_statement IMPORTING io_statement    TYPE REF TO zcl_ppr_statement
                              RETURNING VALUE(rt_rules) TYPE gty_rule_tab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF gty_cache,
        classname TYPE abap_classname,
        instance  TYPE REF TO zif_ppr_formatting_rule,
      END OF gty_cache.
    CLASS-METHODS:
      init.
    CLASS-DATA:
      gt_cache TYPE SORTED TABLE OF gty_cache WITH UNIQUE KEY classname.
ENDCLASS.



CLASS zcl_ppr_rule_factory IMPLEMENTATION.
  METHOD init.
    DATA: li_dummy           TYPE REF TO zif_ppr_formatting_rule,
          lt_implementations TYPE seor_implementing_keys.

    DATA(lo_descr) = CAST cl_abap_intfdescr(
                       CAST cl_abap_refdescr(
                         cl_abap_typedescr=>describe_by_data( li_dummy )
                       )->get_referenced_type( )
                     ).

    CALL FUNCTION 'SEO_INTERFACE_IMPLEM_GET_ALL'
      EXPORTING
        intkey       = VALUE seoclskey( clsname = lo_descr->get_relative_name( ) )
      IMPORTING
        impkeys      = lt_implementations
      EXCEPTIONS
        not_existing = 1
        OTHERS       = 2.
    ASSERT sy-subrc = 0.

    LOOP AT lt_implementations ASSIGNING FIELD-SYMBOL(<ls_implementation>).
      CREATE OBJECT li_dummy TYPE (<ls_implementation>-clsname).
      INSERT VALUE #(
        classname = <ls_implementation>-clsname
        instance  = li_dummy
      ) INTO TABLE gt_cache.
      FREE li_dummy.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_rules_for_context.
    IF gt_cache IS INITIAL.
      init( ).
    ENDIF.

    DATA(lv_classname) = CONV abap_classname(
                           CAST cl_abap_objectdescr(
                             cl_abap_typedescr=>describe_by_object_ref( io_context )
                           )->get_relative_name( )
                         ).

    LOOP AT gt_cache ASSIGNING FIELD-SYMBOL(<ls_cache>).
      DATA(li_rule) = <ls_cache>-instance.

      LOOP AT li_rule->get_formatting_relevancy( ) ASSIGNING FIELD-SYMBOL(<ls_relevancy>).
        CASE <ls_relevancy>-type.
          WHEN zif_ppr_formatting_rule=>gc_relevancy_type-all_contexts.
            APPEND li_rule TO rt_rules.
          WHEN zif_ppr_formatting_rule=>gc_relevancy_type-context_classname.
            IF <ls_relevancy>-classname = lv_classname.
              APPEND li_rule TO rt_rules.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_rules_for_statement.
    IF gt_cache IS INITIAL.
      init( ).
    ENDIF.

    DATA(lv_classname) = CONV abap_classname(
                           CAST cl_abap_objectdescr(
                             cl_abap_typedescr=>describe_by_object_ref( io_statement )
                           )->get_relative_name( )
                         ).

    LOOP AT gt_cache ASSIGNING FIELD-SYMBOL(<ls_cache>).
      DATA(li_rule) = <ls_cache>-instance.

      LOOP AT li_rule->get_formatting_relevancy( ) ASSIGNING FIELD-SYMBOL(<ls_relevancy>).
        CASE <ls_relevancy>-type.
          WHEN zif_ppr_formatting_rule=>gc_relevancy_type-all_statements.
            APPEND li_rule TO rt_rules.
          WHEN zif_ppr_formatting_rule=>gc_relevancy_type-statement_classname.
            IF <ls_relevancy>-classname = lv_classname.
              APPEND li_rule TO rt_rules.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
