"! Chained Statement
CLASS zcl_ppr_chained_statement DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_ppr_statement
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_scan_statement      TYPE REF TO zcl_ppr_scan_statement OPTIONAL
                            iv_base_statement_text TYPE string
                            it_chain_elements      TYPE zcl_ppr_context=>gty_statement_tab OPTIONAL,
      get_base_statement_text RETURNING VALUE(rv_text) TYPE string,
      get_chain_elements RETURNING VALUE(rt_chain_elements) TYPE zcl_ppr_context=>gty_statement_tab,
      get_chain_element IMPORTING iv_index            TYPE i
                        RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_statement,
      set_chain_elements IMPORTING it_chain_elements TYPE zcl_ppr_context=>gty_statement_tab,
      zif_ppr_source_container~get_line_count REDEFINITION,
      zif_ppr_source_container~get_source_code REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mt_chain_elements      TYPE zcl_ppr_context=>gty_statement_tab,
      mv_base_statement_text TYPE string.
ENDCLASS.



CLASS zcl_ppr_chained_statement IMPLEMENTATION.
  METHOD constructor.
    super->constructor( io_scan_statement ).
    mt_chain_elements = it_chain_elements.
  ENDMETHOD.

  METHOD get_base_statement_text.

  ENDMETHOD.

  METHOD get_chain_element.
    READ TABLE mt_chain_elements INDEX iv_index INTO ro_statement.
  ENDMETHOD.

  METHOD get_chain_elements.
    rt_chain_elements = mt_chain_elements.
  ENDMETHOD.

  METHOD set_chain_elements.
    mt_chain_elements = it_chain_elements.
  ENDMETHOD.

  METHOD zif_ppr_source_container~get_line_count.

  ENDMETHOD.

  METHOD zif_ppr_source_container~get_source_code.
    rt_source = VALUE #( ( |{ mv_base_statement_text }: | ) ).
    LOOP AT mt_chain_elements INTO DATA(lo_statement).
      APPEND LINES OF lo_statement->zif_ppr_source_container~get_source_code( ) TO rt_source.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
