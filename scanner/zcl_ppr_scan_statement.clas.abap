"! Scanned Statement
CLASS zcl_ppr_scan_statement DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_ppr_scan_return_value_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING is_statement   TYPE sstmnt
                            iv_id          TYPE i
                            io_scan_result TYPE REF TO zcl_ppr_scan_result,
      get_tokens RETURNING VALUE(rt_tokens) TYPE zcl_ppr_scan_result=>gty_token_object_tab,
      get_structure RETURNING VALUE(ro_structure) TYPE REF TO zcl_ppr_scan_structure,
      get_description REDEFINITION,
      get_statement_type_name RETURNING VALUE(rv_type_name) TYPE string,
      get_statement_text RETURNING VALUE(rv_text) TYPE string.
    DATA:
      ms_statement TYPE sstmnt READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      gc_result_type_name TYPE string VALUE 'Statement'.
ENDCLASS.



CLASS zcl_ppr_scan_statement IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_id               = iv_id
                        io_scan_result      = io_scan_result
                        iv_result_type_name = gc_result_type_name ).
    ms_statement = is_statement.
  ENDMETHOD.

  METHOD get_tokens.
    DO ms_statement-to - ms_statement-from + 1 TIMES.
      APPEND mo_scan_result->get_token_by_id( ms_statement-from + sy-index - 1 ) TO rt_tokens.
    ENDDO.
  ENDMETHOD.

  METHOD get_structure.
    " Get structure of the first token in the statement
    ro_structure = mo_scan_result->get_token_by_id( ms_statement-from )->get_structure( ).
  ENDMETHOD.

  METHOD get_description.
    rv_description = |{ super->get_description( ) }: { get_statement_type_name( ) }|.
  ENDMETHOD.

  METHOD get_statement_type_name.
    CONSTANTS: lc_stmnt_types LIKE zcl_ppr_constants=>gc_scan_stmnt_types
                              VALUE zcl_ppr_constants=>gc_scan_stmnt_types.

    rv_type_name = SWITCH #( ms_statement-type
      WHEN lc_stmnt_types-abap_doc         THEN 'ABAP Doc'
      WHEN lc_stmnt_types-comment          THEN 'Comment'
      WHEN lc_stmnt_types-comment_in_stmnt THEN 'Comment in statement'
      WHEN lc_stmnt_types-compute_direct   THEN 'Compute Direct'
      WHEN lc_stmnt_types-empty            THEN 'Empty'
      WHEN lc_stmnt_types-include          THEN 'Include'
      WHEN lc_stmnt_types-include_miss     THEN 'Include Miss'
      WHEN lc_stmnt_types-macro_call       THEN 'Macro Call'
      WHEN lc_stmnt_types-macro_definition THEN 'Macro Definition'
      WHEN lc_stmnt_types-method_direct    THEN 'Method Direct'
      WHEN lc_stmnt_types-native_sql       THEN 'Native SQL'
      WHEN lc_stmnt_types-opaque_body      THEN 'Opaque Body'
      WHEN lc_stmnt_types-pragma           THEN 'Pragma'
      WHEN lc_stmnt_types-standard         THEN 'Standard'
      WHEN lc_stmnt_types-trmac_call       THEN 'TRMAC Call'
      WHEN lc_stmnt_types-type_pools       THEN 'Type Pools'
      WHEN lc_stmnt_types-type_pools_miss  THEN 'Type Pools Miss'
      WHEN lc_stmnt_types-unknown          THEN 'Unknown'
    ) ##NO_TEXT.
  ENDMETHOD.

  METHOD get_statement_text.
    LOOP AT get_tokens( ) INTO DATA(lo_token).
      rv_text = |{ rv_text } { lo_token->get_token_text( ) }|.
    ENDLOOP.

    rv_text = rv_text && '.'.
  ENDMETHOD.
ENDCLASS.
