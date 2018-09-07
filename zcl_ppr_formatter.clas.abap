"! Formatter
CLASS zcl_ppr_formatter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor,
      format_source IMPORTING it_source           TYPE stringtab
                              io_configuration    TYPE REF TO zcl_ppr_configuration
                              iv_run_standard_pp  TYPE abap_bool DEFAULT abap_true
                    RETURNING VALUE(rt_formatted) TYPE stringtab,
      format_object IMPORTING iv_object_type   TYPE trobjtype
                              iv_object_name   TYPE sobj_name
                              io_configuration TYPE REF TO zcl_ppr_configuration
                    RAISING   zcx_ppr_source_read_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      initialize_processing_session,
      format_context IMPORTING io_context                 TYPE REF TO zcl_ppr_context
                     RETURNING VALUE(rv_rebuild_required) TYPE abap_bool,
      format_statement IMPORTING io_statement               TYPE REF TO zcl_ppr_statement
                       RETURNING VALUE(rv_rebuild_required) TYPE abap_bool.
    DATA:
      mi_source_provider TYPE REF TO zif_ppr_source_provider,
      mo_scanner         TYPE REF TO zcl_ppr_oo_scanner,
      mo_scan_result     TYPE REF TO zcl_ppr_scan_result,
      mo_context         TYPE REF TO zcl_ppr_context,
      mt_original_source TYPE stringtab.
ENDCLASS.



CLASS zcl_ppr_formatter IMPLEMENTATION.


  METHOD constructor.
    mi_source_provider = NEW zcl_ppr_source_provider( ).
    mo_scanner = NEW #( ).
  ENDMETHOD.


  METHOD format_context.
    LOOP AT io_context->get_ordered_components( ) INTO DATA(li_component).
      IF li_component IS INSTANCE OF zcl_ppr_context.
        rv_rebuild_required = format_context( CAST #( li_component ) ).
      ELSEIF li_component IS INSTANCE OF zcl_ppr_statement.
        rv_rebuild_required = format_statement( CAST #( li_component ) ).
      ENDIF.

      IF rv_rebuild_required = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    LOOP AT zcl_ppr_rule_factory=>get_rules_for_context( io_context ) INTO DATA(li_rule).
      rv_rebuild_required = li_rule->apply_rule( ig_settings = '' ir_code = VALUE #( ) io_target = io_context ).
      IF rv_rebuild_required = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD format_object.
    DATA(lt_source) = mi_source_provider->get_source( iv_object_type = iv_object_type
                                                      iv_object_name = iv_object_name
                                                      iv_state       = 'A' ).
    DATA(lt_formatted) = format_source( it_source        = lt_source
                                        io_configuration = io_configuration ).

    LOOP AT lt_formatted ASSIGNING FIELD-SYMBOL(<lv_line>).
      WRITE: / <lv_line>. " TODO remove this
    ENDLOOP.
    ##TODO. " Save
  ENDMETHOD.


  METHOD format_source.
    initialize_processing_session( ).

    IF iv_run_standard_pp = abap_true.
      ##TODO. " Use settings parameter
      CALL FUNCTION 'PRETTY_PRINTER'
        EXPORTING
          inctoo             = abap_false
        TABLES
          ntext              = rt_formatted
          otext              = it_source
        EXCEPTIONS
          enqueue_table_full = 1
          include_enqueued   = 2
          include_readerror  = 3
          include_writeerror = 4
          OTHERS             = 5.
      IF sy-subrc <> 0.
        BREAK-POINT.
      ENDIF.
      mt_original_source = rt_formatted.
    ELSE.
      mt_original_source = it_source.
    ENDIF.

    mo_scan_result = mo_scanner->scan_source( mt_original_source ).
    mo_context = zcl_ppr_context_factory=>get_context_hierarchy_by_scan( mo_scan_result ).

    DATA(lv_done) = abap_false.
    WHILE lv_done = abap_false.
      IF sy-index > 999.
        ASSERT 1 = 2. " o0
      ENDIF.
      lv_done = boolc( NOT format_context( mo_context ) ).
      IF lv_done = abap_false.
        mo_scan_result = mo_scanner->scan_source( mt_original_source ).
        mo_context = zcl_ppr_context_factory=>get_context_hierarchy_by_scan( mo_scan_result ).
      ENDIF.
    ENDWHILE.

    rt_formatted = mo_context->get_source_code( ).

*    rt_formatted = mo_context->format( io_configuration ).
  ENDMETHOD.


  METHOD format_statement.
    LOOP AT zcl_ppr_rule_factory=>get_rules_for_statement( io_statement ) INTO DATA(lo_rule).
      rv_rebuild_required = lo_rule->apply_rule( ig_settings = '' ir_code = VALUE #( ) io_target = io_statement ).
      IF rv_rebuild_required = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD initialize_processing_session.
    CLEAR: mt_original_source.
    FREE: mo_scan_result,
          mo_context.
  ENDMETHOD.
ENDCLASS.
