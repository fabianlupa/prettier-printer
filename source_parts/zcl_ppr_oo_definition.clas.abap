"! OO definition
CLASS zcl_ppr_oo_definition DEFINITION
  PUBLIC
  ABSTRACT
  INHERITING FROM zcl_ppr_statement_parent
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_keyword  TYPE string
                            it_source   TYPE stringtab
                            it_tokens   TYPE stokesx_tab
                            it_children TYPE zcl_ppr_statement_parent=>gty_children OPTIONAL.
    DATA:
      "! Global definition
      mv_global     TYPE abap_bool READ-ONLY,
      "! Class / Interface name
      mv_identifier TYPE string READ-ONLY.
  PROTECTED SECTION.
    METHODS:
      parse_information REDEFINITION,
      parse_identifier ABSTRACT RETURNING VALUE(rv_identifier) TYPE string.
  PRIVATE SECTION.
    METHODS:
      parse_global RETURNING VALUE(rv_is_global) TYPE abap_bool.
ENDCLASS.



CLASS zcl_ppr_oo_definition IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_keyword  = iv_keyword
                        it_source   = it_source
                        it_tokens   = it_tokens
                        it_children = it_children ).
  ENDMETHOD.

  METHOD parse_information.
    super->parse_information( ).

    mv_identifier = parse_identifier( ).
    mv_global = parse_global( ).
  ENDMETHOD.

  METHOD parse_global.
    LOOP AT get_tokens( ) ASSIGNING FIELD-SYMBOL(<ls_token>)
                          WHERE type = '?'.
      IF <ls_token>-str = 'PUBLIC' ##TODO.
        rv_is_global = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    rv_is_global = abap_false.
  ENDMETHOD.
ENDCLASS.
