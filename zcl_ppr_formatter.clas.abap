"! Formatter
CLASS zcl_ppr_formatter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      format_source IMPORTING it_source           TYPE stringtab
                              iv_run_standard_pp  TYPE abap_bool DEFAULT abap_true
                    RETURNING VALUE(rt_formatted) TYPE stringtab,
      format_object IMPORTING iv_object_type TYPE trobjtype
                              iv_object_name TYPE sobj_name.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      initialize_processing_session.
    DATA:
      mt_original_source TYPE stringtab.
ENDCLASS.



CLASS zcl_ppr_formatter IMPLEMENTATION.
  METHOD format_object.

  ENDMETHOD.

  METHOD format_source.
    initialize_processing_session( ).

    IF iv_run_standard_pp = abap_true.
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
  ENDMETHOD.

  METHOD initialize_processing_session.
    CLEAR: mt_original_source.
  ENDMETHOD.
ENDCLASS.
