"! Statement factory
CLASS zcl_ppr_statement_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING it_statements TYPE sstmnt_tab
                            it_tokens     TYPE stokesx_tab
                            it_structures TYPE sstruc_tab,
      create_statement_recursive IMPORTING iv_statement_index  TYPE i
                                 EXPORTING ev_next_statement   TYPE i
                                 RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_statement.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS:
      is_multi_level_statement IMPORTING is_statement              TYPE sstmnt
                               RETURNING VALUE(rv_multiple_levels) TYPE abap_bool.
    METHODS:
      get_tokens_for_statement IMPORTING is_statement     TYPE sstmnt
                               RETURNING VALUE(rt_tokens) TYPE stokesx_tab,
      determine_end_statement IMPORTING iv_statement_index     TYPE i
                              EXPORTING ev_end_statement_index TYPE i
                                        ev_implicit_end        TYPE abap_bool,
      get_statement_by_index IMPORTING iv_index            TYPE i
                             RETURNING VALUE(rr_statement) TYPE REF TO sstmnt,
      get_structure_by_statement IMPORTING ir_statement        TYPE REF TO sstmnt
                                 RETURNING VALUE(rr_structure) TYPE REF TO sstruc,
      create_statement IMPORTING ir_statement        TYPE sstmnt
                                 it_tokens           TYPE stokesx_tab
                                 is_structure        TYPE sstruc
                                 it_children TYPE zcl_ppr_statement_parent=>gty_children
                       RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_statement.
    DATA:
      mt_statements TYPE sstmnt_tab,
      mt_tokens     TYPE stokesx_tab,
      mt_structures TYPE sstruc_tab.
ENDCLASS.



CLASS zcl_ppr_statement_factory IMPLEMENTATION.
  METHOD constructor.
    mt_statements = it_statements.
    mt_tokens = it_tokens.
    mt_structures = it_structures.
  ENDMETHOD.

  METHOD create_statement.

  ENDMETHOD.

  METHOD create_statement_recursive.
    DATA: lt_children               TYPE STANDARD TABLE OF REF TO zcl_ppr_statement,
          lo_explicit_end_statement TYPE REF TO zcl_ppr_statement.

    ev_next_statement = iv_statement_index + 1.

    DATA(lr_statement) = get_statement_by_index( iv_statement_index ).

*    DATA(lr_statement) = REF #( mt_statements[ iv_statement_index ] ).
*    DATA(lr_structure) = REF #( mt_structures[ mt_tokens[ lr_statement->from ]-row ] ).
*    DATA(lt_tokens) = get_tokens_for_statement( lr_statement->* ).

    IF is_multi_level_statement( lr_statement->* ) = abap_true.
      determine_end_statement(
        EXPORTING
          iv_statement_index = iv_statement_index
        IMPORTING
          ev_end_statement_index = DATA(lv_end_statement_index)
          ev_implicit_end        = DATA(lv_implicit_end)
      ).

      ev_next_statement = COND #( WHEN lv_implicit_end = abap_false
                                  THEN lv_end_statement_index + 1
                                  ELSE lv_end_statement_index ).

      DO lv_end_statement_index - iv_statement_index TIMES.
        APPEND create_statement_recursive(
          iv_statement_index = sy-index + iv_statement_index
        ) TO lt_children.
      ENDDO.

      IF lv_implicit_end = abap_false.
        lo_explicit_end_statement = create_statement_recursive( lv_end_statement_index ).
      ENDIF.

*      ro_statement = NEW zcl_ppr_oo_cl
    ELSE.

    ENDIF.
  ENDMETHOD.

  METHOD is_multi_level_statement.
*    rv_multiple_levels = SWITCH #( is_statement-t)
  ENDMETHOD.

  METHOD get_tokens_for_statement.
    LOOP AT mt_tokens FROM is_statement-from TO is_statement-to ASSIGNING FIELD-SYMBOL(<ls_token>).
      APPEND <ls_token> TO rt_tokens.
    ENDLOOP.
  ENDMETHOD.

  METHOD determine_end_statement.

  ENDMETHOD.

  METHOD get_statement_by_index.
    rr_statement = REF #( mt_statements[ iv_index ] ).
  ENDMETHOD.

  METHOD get_structure_by_statement.
    rr_structure = REF #( mt_structures[ mt_tokens[ ir_statement->from ]-row ] ).
  ENDMETHOD.
ENDCLASS.
