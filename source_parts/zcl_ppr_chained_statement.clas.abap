"! Chained statement
CLASS zcl_ppr_chained_statement DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF gty_part,
        index      TYPE i,
        identifier TYPE string,
        statement  TYPE REF TO zcl_ppr_statement,
      END OF gty_part,
      gty_part_tab TYPE SORTED TABLE OF gty_part
                        WITH UNIQUE KEY index
                        WITH UNIQUE HASHED KEY ref_unique_key COMPONENTS statement.
    CLASS-METHODS:
      build_chain IMPORTING it_statement    TYPE gty_part_tab
                  RETURNING VALUE(ro_chain) TYPE REF TO zcl_ppr_chained_statement.
    METHODS:
      constructor IMPORTING iv_keyword TYPE sana_word,
      apply_configuration IMPORTING io_config TYPE REF TO zcl_ppr_configuration,
      sort_parts_by_identifier,
      add_part IMPORTING io_statement TYPE REF TO zcl_ppr_statement,
      remove_part IMPORTING io_statement TYPE REF TO zcl_ppr_statement,
      remove_part_by_index IMPORTING iv_index TYPE i,
      get_part_amount RETURNING VALUE(rv_parts) TYPE i.
    DATA:
      mv_keyword TYPE sana_word READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mt_parts TYPE gty_part_tab.
ENDCLASS.



CLASS zcl_ppr_chained_statement IMPLEMENTATION.
  METHOD add_part.
    TRY.
        INSERT VALUE #(
          index = get_part_amount( ) + 1
          identifier = io_statement->get_identifier( )
          statement = io_statement
        ) INTO TABLE mt_parts.
      CATCH cx_sy_itab_duplicate_key INTO DATA(lx_ex).
*      RAISE E
    ENDTRY.
  ENDMETHOD.

  METHOD build_chain.

  ENDMETHOD.

  METHOD remove_part.
    TRY.
        DATA(lr_line) = REF #( mt_parts[ KEY ref_unique_key statement = io_statement ] ).
        DELETE TABLE mt_parts FROM lr_line->*.
      CATCH cx_sy_itab_line_not_found INTO DATA(lx_ex).
*      RAISE
    ENDTRY.
  ENDMETHOD.

  METHOD remove_part_by_index.

  ENDMETHOD.

  METHOD sort_parts_by_identifier.
    DATA: lt_parts TYPE STANDARD TABLE OF gty_part.

    lt_parts = mt_parts.
    SORT lt_parts BY identifier ASCENDING.
    LOOP AT lt_parts ASSIGNING FIELD-SYMBOL(<ls_part>).
      <ls_part>-index = sy-tabix.
    ENDLOOP.
    mt_parts = lt_parts.
  ENDMETHOD.

  METHOD constructor.
    mv_keyword = iv_keyword.
  ENDMETHOD.

  METHOD get_part_amount.
    rv_parts = lines( mt_parts ).
  ENDMETHOD.

  METHOD apply_configuration.

  ENDMETHOD.
ENDCLASS.
