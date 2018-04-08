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
    TYPES:
      BEGIN OF gty_processor,
        order     TYPE i,
        processor TYPE REF TO zcl_ppr_processor_base,
      END OF gty_processor.
    CLASS-METHODS:
      scan_source IMPORTING it_source     TYPE stringtab
                  EXPORTING et_tokens     TYPE stokesx_tab
                            et_statements TYPE sstmnt_tab
                            et_structures TYPE sstruc_tab.
    METHODS:
      initialize_processing_session,
      split_source_into_processors,
      get_source_from_to IMPORTING iv_from          TYPE i
                                   iv_to            TYPE i
                         RETURNING VALUE(rt_source) TYPE stringtab,
      build_source_hierarchy.
    DATA:
      mt_tokens          TYPE stokesx_tab,
      mt_statements      TYPE sstmnt_tab,
      mt_structures      TYPE sstruc_tab,
      mt_original_source TYPE stringtab,
      mt_processors      TYPE SORTED TABLE OF gty_processor WITH UNIQUE KEY order.
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

    " Assume normal pretty printer has been executed, this means the following should
    " already be as the user expects it:
    " - Intendation
    " - Upper/Lower Case Conversion

    scan_source(
      EXPORTING
        it_source     = mt_original_source
      IMPORTING
        et_tokens     = mt_tokens
        et_statements = mt_statements
        et_structures = mt_structures
    ).

    build_source_hierarchy( ).

    split_source_into_processors( ).

    LOOP AT mt_processors ASSIGNING FIELD-SYMBOL(<ls_processor>).
      <ls_processor>-processor->process( ).
      APPEND LINES OF <ls_processor>-processor->get_result( ) TO rt_formatted.
    ENDLOOP.
  ENDMETHOD.

  METHOD initialize_processing_session.
    CLEAR: mt_statements,
           mt_tokens,
           mt_structures,
           mt_processors.
  ENDMETHOD.

  METHOD split_source_into_processors.
    DATA(lv_statement_count) = lines( mt_statements ).
    DATA(lv_counter) = 1.
    WHILE lv_counter < lv_statement_count.
      TRY.
          DATA(lr_statement) = REF #( mt_statements[ lv_counter ] ).
          " Find out structure information for the token in the statement
          DATA(lr_structure) = REF #( mt_structures[ mt_tokens[ lr_statement->from ]-row ] ).
          CASE lr_structure->stmnt_type.
            WHEN scan_struc_stmnt_type-class_definition.
              " Give over processing for the whole class definition part
              INSERT VALUE #(
                order = lines( mt_processors ) + 1
                processor = NEW zcl_ppr_classdef_processor(
                              get_source_from_to( iv_from = lr_statement->from
                                                  iv_to   = lr_statement->to )
                            )
              ) INTO TABLE mt_processors.
              lv_counter = lr_statement->to + 1.
              CONTINUE.
*            WHEN scan_struc_stmnt_type-method.
*              BREAK-POINT.
            WHEN OTHERS.
              " Unsupported, leave as is
              INSERT VALUE #(
                order = lines( mt_processors ) + 1
                processor = NEW zcl_ppr_dummy_processor(
                              get_source_from_to( iv_from = lr_statement->from
                                                  iv_to   = lr_statement->to )
                            )
              ) INTO TABLE mt_processors.
              lv_counter = lr_statement->to + 1.
              CONTINUE.
*              BREAK-POINT.
          ENDCASE.
        CATCH cx_sy_itab_line_not_found.
          BREAK-POINT.
      ENDTRY.

      ADD 1 TO lv_counter.
    ENDWHILE.
  ENDMETHOD.

  METHOD build_source_hierarchy.
    DATA(lv_statement_count) = lines( mt_statements ).
    DATA(lv_counter) = 1.

    WHILE lv_counter <= lv_statement_count.
*      DATA(lr_statement) = REF #( mt_statements[ lv_counter ] ).
*      DATA(lr_structure) = REF #( mt_structures[ mt_tokens[ lr_statement->from ]-row ] ).
*      DATA(lt_tokens) = get_tokens_for_statement( lr_statement->* ).
*
*      zcl_ppr_statement_factory=>
*
*      ADD 1 TO lv_counter.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_source_from_to.
    LOOP AT mt_original_source FROM iv_from TO iv_to ASSIGNING FIELD-SYMBOL(<lv_source>).
      APPEND <lv_source> TO rt_source.
    ENDLOOP.
  ENDMETHOD.

  METHOD scan_source.
    SCAN ABAP-SOURCE it_source
      TOKENS INTO et_tokens
      STATEMENTS INTO et_statements
      STRUCTURES INTO et_structures
      WITH ANALYSIS
      WITH COMMENTS
      WITH BLOCKS
      WITH DECLARATIONS
      WITH PRAGMAS '*'
      PRESERVING IDENTIFIER ESCAPING
      WITHOUT TRMAC.
    BREAK-POINT.
  ENDMETHOD.
ENDCLASS.
