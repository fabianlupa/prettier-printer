"! Scanned Statement
CLASS zcl_ppr_scan_statement DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_ppr_scan_return_value_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      gc_result_type_name TYPE string VALUE 'STATEMENT'.
    METHODS:
      constructor IMPORTING is_statement   TYPE sstmnt
                            iv_id          TYPE i
                            io_scan_result TYPE REF TO zcl_ppr_scan_result,
      get_tokens RETURNING VALUE(rt_tokens) TYPE zcl_ppr_scan_result=>gty_token_object_tab,
      get_token IMPORTING iv_index        TYPE i
                RETURNING VALUE(ro_token) TYPE REF TO zcl_ppr_scan_token,
      get_structure RETURNING VALUE(ro_structure) TYPE REF TO zcl_ppr_scan_structure,
      get_description REDEFINITION,
      get_statement_type RETURNING VALUE(rv_type) TYPE stmnt_type,
      get_statement_type_name RETURNING VALUE(rv_type_name) TYPE string,
      get_statement_text RETURNING VALUE(rv_text) TYPE string,
      get_first_line_number RETURNING VALUE(rv_line) TYPE i,
      get_last_line_number RETURNING VALUE(rv_line) TYPE i,
      get_source RETURNING VALUE(rt_source) TYPE stringtab,
      is_part_of_chained_statement RETURNING VALUE(rv_is_chained) TYPE abap_bool,
      is_statement_first_in_line RETURNING VALUE(rv_first) TYPE abap_bool,
      get_dot_position RETURNING VALUE(rv_position) TYPE i.
    DATA:
      ms_statement TYPE sstmnt READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_scan_statement IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_id               = iv_id
                        io_scan_result      = io_scan_result
                        iv_result_type_name = gc_result_type_name ).
    ms_statement = is_statement.
  ENDMETHOD.

  METHOD get_tokens.
    DATA(lv_from) = COND #( WHEN ms_statement-from < ms_statement-to
                            THEN ms_statement-from ELSE ms_statement-to ).
    DATA(lv_to) = COND #( WHEN ms_statement-to > ms_statement-from
                          THEN ms_statement-to ELSE ms_statement-from ).
    DO lv_to - lv_from + 1 TIMES.
      APPEND mo_scan_result->get_token_by_id( lv_from + sy-index - 1 ) TO rt_tokens.
    ENDDO.
  ENDMETHOD.

  METHOD get_token.
    DATA(lt_tokens) = get_tokens( ).
    ro_token = lt_tokens[ iv_index ].
  ENDMETHOD.

  METHOD get_structure.
    ro_structure = mo_scan_result->get_structure_by_id( ms_statement-struc ).
  ENDMETHOD.

  METHOD get_description.
    rv_description = |{ super->get_description( ) }: { get_statement_type_name( ) }|.
  ENDMETHOD.

  METHOD get_statement_type.
    rv_type = ms_statement-type.
  ENDMETHOD.

  METHOD get_statement_type_name.
    CONSTANTS: lc_stmnt_types LIKE zcl_ppr_constants=>gc_scan_stmnt_types
                              VALUE zcl_ppr_constants=>gc_scan_stmnt_types.

    rv_type_name = SWITCH #( get_statement_type( )
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
      ELSE 'Unknown'
    ) ##NO_TEXT.
  ENDMETHOD.

  METHOD get_statement_text.
    DATA: lt_token_texts TYPE stringtab.

    LOOP AT get_tokens( ) INTO DATA(lo_token).
      APPEND lo_token->get_token_text( ) TO lt_token_texts.
    ENDLOOP.

    CONCATENATE LINES OF lt_token_texts INTO rv_text SEPARATED BY ` `.
  ENDMETHOD.

  METHOD get_first_line_number.
    DATA(lt_tokens) = get_tokens( ).
    rv_line = lt_tokens[ 1 ]->get_row( ).
  ENDMETHOD.

  METHOD get_last_line_number.
    DATA(lt_tokens) = get_tokens( ).
    rv_line = lt_tokens[ lines( lt_tokens ) ]->get_row( ).
  ENDMETHOD.

  METHOD get_source.
*    DATA(lt_tokens) = get_tokens( ).

    ##TODO. " This will give wrong results with chained statements and multiple statements per line
    LOOP AT mo_scan_result->mt_source FROM get_first_line_number( ) TO get_last_line_number( )
                                      INTO DATA(lv_source_line).
*      AT FIRST.
*        lt_tokens[ 1 ]->
*      ENDAT.
*
*      AT LAST.
*
*      ENDAT.

      APPEND lv_source_line TO rt_source.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_part_of_chained_statement.
    rv_is_chained = boolc( ms_statement-colonrow IS NOT INITIAL ).
  ENDMETHOD.

  METHOD is_statement_first_in_line.
    LOOP AT mo_scan_result->get_statements_by_line( get_first_line_number( ) ) INTO DATA(lo_statement).
      rv_first = boolc( lo_statement->mv_id = mv_id ).
      EXIT.
    ENDLOOP.
    ASSERT sy-subrc = 0.
  ENDMETHOD.

  METHOD get_dot_position.
    rv_position = ms_statement-tcol.
  ENDMETHOD.
ENDCLASS.
