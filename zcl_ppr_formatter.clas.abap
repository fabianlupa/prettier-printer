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
      initialize_processing_session.
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

  METHOD format_object.
    DATA(lt_source) = mi_source_provider->get_source( iv_object_type = iv_object_type
                                                      iv_object_name = iv_object_name
                                                      iv_state       = 'A' ).
    format_source( it_source        = lt_source
                   io_configuration = io_configuration ).
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
    mo_context = zcl_ppr_hierarchy_factory=>get_context_hierarchy_by_scan( mo_scan_result ).

    rt_formatted = mo_context->format( io_configuration ).
  ENDMETHOD.

  METHOD initialize_processing_session.
    CLEAR: mt_original_source.
    FREE: mo_scan_result,
          mo_context.
  ENDMETHOD.
ENDCLASS.
