"! Class definition
CLASS zcl_ppr_oo_class_definition DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_ppr_oo_definition
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      gc_keyword TYPE string VALUE 'CLASS DEFINITION'.
    METHODS:
      constructor IMPORTING it_source   TYPE stringtab
                            it_tokens   TYPE stokesx_tab
                            it_children TYPE zcl_ppr_statement_parent=>gty_children OPTIONAL,
      get_public_section RETURNING VALUE(ro_section) TYPE REF TO zcl_ppr_class_section,
      get_protected_section RETURNING VALUE(ro_section) TYPE REF TO zcl_ppr_class_section,
      get_private_section RETURNING VALUE(ro_section) TYPE REF TO zcl_ppr_class_section,
      get_package_section RETURNING VALUE(ro_section) TYPE REF TO zcl_ppr_class_section,
      get_classname RETURNING VALUE(rv_classname) TYPE abap_classname.
  PROTECTED SECTION.
    METHODS:
      parse_identifier REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_oo_class_definition IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_keyword  = gc_keyword
                        it_source   = it_source
                        it_tokens   = it_tokens
                        it_children = it_children ).
  ENDMETHOD.

  METHOD get_classname.
    rv_classname = EXACT #( mv_identifier ).
  ENDMETHOD.

  METHOD get_public_section.

  ENDMETHOD.

  METHOD parse_identifier.
    LOOP AT get_tokens( ) ASSIGNING FIELD-SYMBOL(<ls_token>) ##TODO.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_package_section.
    ASSERT 1 = 2. " Please come back package visibility concept
  ENDMETHOD.

  METHOD get_private_section.

  ENDMETHOD.

  METHOD get_protected_section.

  ENDMETHOD.

ENDCLASS.
