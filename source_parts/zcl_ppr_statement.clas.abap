"! Statement
CLASS zcl_ppr_statement DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_keyword TYPE string OPTIONAL
                            it_source  TYPE stringtab
                            it_tokens  TYPE stokesx_tab,
      apply_formatting_rule
*      IMPORTING io_rule TYPE REF TO zcl_ppr_rule
      .
    DATA:
      "! Keyword for the statement
      "! <p>
      "! The keyword will also be filled if an alternative variation without keyword is used. For
      "! example 'CALL METHOD' will be set with both the keyword based and the functional call
      "! variant.
      "! </p>
      mv_keyword                   TYPE string READ-ONLY,
      "! Statement does not use its assigned keyword
      mv_without_keyword_variation TYPE abap_bool READ-ONLY,
      "! Original source code
      mt_original_source           TYPE stringtab READ-ONLY,
      "! Current source code with all applied operations
      mv_current_source_line       TYPE string READ-ONLY.
  PROTECTED SECTION.
    METHODS:
      parse_information,
      get_tokens FINAL RETURNING VALUE(rt_tokens) TYPE stokesx_tab.
  PRIVATE SECTION.
    DATA:
      mt_tokens TYPE stokesx_tab.
ENDCLASS.



CLASS zcl_ppr_statement IMPLEMENTATION.
  METHOD constructor.
    mt_original_source = it_source.
    mv_keyword = iv_keyword.
    mt_tokens = it_tokens.

    parse_information( ).
  ENDMETHOD.

  METHOD apply_formatting_rule.

  ENDMETHOD.

  METHOD parse_information ##NEEDED.
  ENDMETHOD.

  METHOD get_tokens.
    rt_tokens = mt_tokens.
  ENDMETHOD.
ENDCLASS.
