"! Statement with children
CLASS zcl_ppr_statement_parent DEFINITION
  PUBLIC
  INHERITING FROM zcl_ppr_statement
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      gty_children TYPE STANDARD TABLE OF REF TO zcl_ppr_statement.
    METHODS:
      constructor IMPORTING iv_keyword  TYPE string
                            it_source   TYPE stringtab
                            it_tokens   TYPE stokesx_tab
                            it_children TYPE gty_children OPTIONAL.
    DATA:
      mt_children TYPE gty_children READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_statement_parent IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_keyword = iv_keyword
                        it_source  = it_source
                        it_tokens  = it_tokens ).
    mt_children = it_children.
  ENDMETHOD.
ENDCLASS.
