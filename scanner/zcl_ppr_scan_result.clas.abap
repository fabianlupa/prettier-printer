"! Scan Result
CLASS zcl_ppr_scan_result DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      gty_statement_object_tab TYPE STANDARD TABLE OF REF TO zcl_ppr_scan_statement WITH DEFAULT KEY,
      gty_token_object_tab     TYPE STANDARD TABLE OF REF TO zcl_ppr_scan_token WITH DEFAULT KEY,
      gty_structure_object_tab TYPE STANDARD TABLE OF REF TO zcl_ppr_scan_structure WITH DEFAULT KEY.
    METHODS:
      set_all IMPORTING it_statements        TYPE sstmnt_tab
                        it_tokens            TYPE stokesx_tab
                        it_structures        TYPE sstruc_tab
                        it_source            TYPE stringtab
                        it_statement_objects TYPE gty_statement_object_tab
                        it_token_objects     TYPE gty_token_object_tab
                        it_structure_objects TYPE gty_structure_object_tab,
      get_statement_by_token IMPORTING io_token            TYPE REF TO zcl_ppr_scan_token
                             RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_scan_statement,
      get_token_by_id IMPORTING iv_id           TYPE i
                      RETURNING VALUE(ro_token) TYPE REF TO zcl_ppr_scan_token,
      get_structure_by_id IMPORTING iv_id               TYPE i
                          RETURNING VALUE(ro_structure) TYPE REF TO zcl_ppr_scan_structure.
    DATA:
      mt_source     TYPE stringtab READ-ONLY,
      mt_statements TYPE gty_statement_object_tab READ-ONLY,
      mt_tokens     TYPE gty_token_object_tab READ-ONLY,
      mt_structures TYPE gty_structure_object_tab READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
*    DATA:
*      mt_statements        TYPE sstmnt_tab READ-ONLY,
*      mt_tokens            TYPE stokesx_tab READ-ONLY,
*      mt_structures        TYPE sstruc_tab READ-ONLY,
ENDCLASS.



CLASS zcl_ppr_scan_result IMPLEMENTATION.
  METHOD set_all.
*    mt_statements = it_statements.
*    mt_tokens = it_tokens.
*    mt_structures = it_structures.
    mt_source = it_source.
    mt_statements = it_statement_objects.
    mt_tokens = it_token_objects.
    mt_structures = it_structure_objects.
  ENDMETHOD.

  METHOD get_statement_by_token.
    DATA(lv_token_id) = line_index( mt_tokens[ table_line = io_token ] ).
    LOOP AT mt_statements INTO DATA(lo_statement).
      IF ro_statement->ms_statement-from >= lv_token_id AND
         ro_statement->ms_statement-to <= lv_token_id.
        ro_statement = lo_statement.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_token_by_id.
    ro_token = mt_tokens[ iv_id ].
  ENDMETHOD.

  METHOD get_structure_by_id.
    ro_structure = mt_structures[ iv_id ].
  ENDMETHOD.
ENDCLASS.
