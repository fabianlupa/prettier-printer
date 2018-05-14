"! Facade of SCAN ABAP-SOURCE statement
CLASS zcl_ppr_source_scanner DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES:
      zif_ppr_source_scanner.
    ALIASES:
      scan_source FOR zif_ppr_source_scanner~scan_source.
    CLASS-METHODS:
      get_instance RETURNING VALUE(ri_instance) TYPE REF TO zif_ppr_source_scanner.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_source_scanner IMPLEMENTATION.
  METHOD get_instance.
    STATICS: so_instance TYPE REF TO zcl_ppr_source_scanner.

    IF so_instance IS NOT BOUND.
      so_instance = NEW #( ).
    ENDIF.

    ri_instance = so_instance.
  ENDMETHOD.

  METHOD zif_ppr_source_scanner~scan_source.
    SCAN ABAP-SOURCE it_source
      TOKENS INTO et_tokens
      STATEMENTS INTO et_statements
      STRUCTURES INTO et_structures
      PRESERVING IDENTIFIER ESCAPING
      WITH ANALYSIS
      WITH COMMENTS
      WITH PRAGMAS '*'
      WITHOUT TRMAC.
    IF sy-subrc <> 0.
      ASSERT 1 = 2 ##TODO.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
