CLASS ltc_simple_code DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    METHODS:
      report FOR TESTING.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      setup,
      teardown.
    DATA:
      mo_scanner TYPE REF TO zcl_ppr_oo_scanner.
ENDCLASS.

CLASS ltc_simple_code IMPLEMENTATION.
  METHOD setup.
    " One could mock SCAN ABAP-SOURCE here, but seems unreasonable
    mo_scanner = NEW #( zcl_ppr_source_scanner=>get_instance( ) ).
  ENDMETHOD.

  METHOD teardown.
    FREE mo_scanner.
  ENDMETHOD.

  METHOD report.
    DATA(lt_source) = VALUE stringtab(
      ( |REPORT ztest.| )
      ( || )
      ( |TABLES t000.| )
      ( || )
      ( |PARAMETERS p_1 TYPE abap_bool AS CHECKBOX.| )
      ( |SELECT-OPTIONS s_1 FOR t000-mandt.| )
      ( || )
      ( |START-OF-SELECTION.| )
      ( |  WRITE 'Test'.| )
    ).

    DATA(lo_result) = mo_scanner->scan_source( lt_source ).

    cl_abap_unit_assert=>assert_equals( exp = 6 act = lines( lo_result->mt_statements ) ).
    cl_abap_unit_assert=>assert_equals( exp = 17 act = lines( lo_result->mt_tokens ) ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( lo_result->mt_structures ) ).

    LOOP AT lo_result->mt_statements INTO DATA(lo_statement).
      DATA(lv_expected_token_sequence) = SWITCH string( lo_statement->mv_id
        WHEN 1 THEN 'REPORT ZTEST'
        WHEN 2 THEN 'TABLES T000'
        WHEN 3 THEN 'PARAMETERS P_1 TYPE ABAP_BOOL AS CHECKBOX'
        WHEN 4 THEN 'SELECT-OPTIONS S_1 FOR T000-MANDT'
        WHEN 5 THEN 'START-OF-SELECTION'
        WHEN 6 THEN 'WRITE ''Test'''
        ELSE '?!?!??!?'
      ).
      cl_abap_unit_assert=>assert_equals( exp = lv_expected_token_sequence
                                          act = lo_statement->get_statement_text( ) ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
