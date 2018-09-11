"! Statement factory
CLASS zcl_ppr_statement_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_statement_from_scan IMPORTING io_scan_statement   TYPE REF TO zcl_ppr_scan_statement
                              RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_statement,
      get_chained_stmnt_from_scan IMPORTING it_scan_statements          TYPE zcl_ppr_scan_result=>gty_statement_object_tab
                                  RETURNING VALUE(ro_chained_statement) TYPE REF TO zcl_ppr_chained_statement.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_statement_factory IMPLEMENTATION.
  METHOD get_statement_from_scan.
    CASE io_scan_statement->get_statement_type( ).
      WHEN zcl_ppr_constants=>gc_scan_stmnt_types-comment.
    ENDCASE.

    CASE io_scan_statement->get_structure( )->get_structure_type( ).
      WHEN zcl_ppr_constants=>gc_scan_struc_types-alternation.
        ro_statement = NEW zcl_ppr_stmnt_condition( io_scan_statement ).

      WHEN zcl_ppr_constants=>gc_scan_struc_types-declaration.
        BREAK-POINT ##TODO.
        IF io_scan_statement->get_token( 1 )->get_token_text( ) = 'TYPES'.
          ro_statement = NEW zcl_ppr_stmnt_typedef( io_scan_statement ).
        ENDIF.
*      WHEN zcl_ppr_constants=>gc_scan_stmnt_types-standard.

      WHEN OTHERS.
        CASE io_scan_statement->get_token( 1 )->get_token_text( ).
          WHEN 'TYPES'.
            ro_statement = NEW zcl_ppr_stmnt_typedef( io_scan_statement ).
          WHEN 'METHODS'.
            ro_statement = NEW zcl_ppr_stmnt_methdef( io_scan_statement ).
        ENDCASE.
    ENDCASE.

    IF ro_statement IS NOT BOUND.
      ro_statement = NEW #( io_scan_statement ).
    ENDIF.
  ENDMETHOD.

  METHOD get_chained_stmnt_from_scan.
    DATA: lt_base_token_texts TYPE stringtab,
          lt_chain_parts      TYPE zcl_ppr_context=>gty_statement_tab.

    LOOP AT it_scan_statements INTO DATA(lo_statement).
      IF sy-tabix = 1.
        DO lo_statement->get_token_count_before_colon( ) TIMES.
          APPEND lo_statement->get_token( sy-index )->get_token_text( ) TO lt_base_token_texts.
        ENDDO.
        CONCATENATE LINES OF lt_base_token_texts INTO DATA(lv_base_statement_text) SEPARATED BY space.
        ro_chained_statement = NEW zcl_ppr_chained_statement( iv_base_statement_text = lv_base_statement_text ).
      ENDIF.

      ASSERT lo_statement->is_part_of_chained_statement( ) = abap_true.
      APPEND get_statement_from_scan( lo_statement ) TO lt_chain_parts.
    ENDLOOP.

    ro_chained_statement->set_chain_elements( lt_chain_parts ).
  ENDMETHOD.
ENDCLASS.
