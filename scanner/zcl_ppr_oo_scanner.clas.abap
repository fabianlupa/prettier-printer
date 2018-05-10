CLASS zcl_ppr_oo_scanner DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING ii_scanner TYPE REF TO zif_ppr_source_scanner,
      scan_source IMPORTING it_source        TYPE stringtab
                  RETURNING VALUE(ro_result) TYPE REF TO zcl_ppr_scan_result.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mi_scanner TYPE REF TO zif_ppr_source_scanner.
ENDCLASS.



CLASS zcl_ppr_oo_scanner IMPLEMENTATION.
  METHOD constructor.
    ASSERT ii_scanner IS BOUND ##TODO.
    mi_scanner = ii_scanner.
  ENDMETHOD.

  METHOD scan_source.
    DATA: lt_token_objects     TYPE STANDARD TABLE OF REF TO zcl_ppr_scan_token,
          lt_statement_objects TYPE STANDARD TABLE OF REF TO zcl_ppr_scan_statement,
          lt_structure_objects TYPE STANDARD TABLE OF REF TO zcl_ppr_scan_structure.

    mi_scanner->scan_source(
      EXPORTING
        it_source     = it_source
      IMPORTING
        et_tokens     = DATA(lt_tokens)
        et_statements = DATA(lt_statements)
        et_structures = DATA(lt_structures)
    ).

    ro_result = NEW #( ).

    LOOP AT lt_tokens ASSIGNING FIELD-SYMBOL(<ls_token>).
      APPEND NEW zcl_ppr_scan_token( is_token       = <ls_token>
                                     iv_id          = sy-tabix
                                     io_scan_result = ro_result ) TO lt_token_objects.
    ENDLOOP.

    LOOP AT lt_statements ASSIGNING FIELD-SYMBOL(<ls_statement>).
      APPEND NEW zcl_ppr_scan_statement( is_statement   = <ls_statement>
                                         iv_id          = sy-tabix
                                         io_scan_result = ro_result ) TO lt_statement_objects.
    ENDLOOP.

    LOOP AT lt_structures ASSIGNING FIELD-SYMBOL(<ls_structure>).
      APPEND NEW zcl_ppr_scan_structure( is_structure   = <ls_structure>
                                         iv_id          = sy-tabix
                                         io_scan_result = ro_result ) TO lt_structure_objects.
    ENDLOOP.

    ro_result->set_all( it_tokens            = lt_tokens
                        it_statements        = lt_statements
                        it_structures        = lt_structures
                        it_source            = it_source
                        it_statement_objects = lt_statement_objects
                        it_token_objects     = lt_token_objects
                        it_structure_objects = lt_structure_objects ).
  ENDMETHOD.
ENDCLASS.
