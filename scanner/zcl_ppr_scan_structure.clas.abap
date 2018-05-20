"! Scanned Structure
CLASS zcl_ppr_scan_structure DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_ppr_scan_return_value_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      gc_result_type_name TYPE string VALUE 'STRUCTURE'.
    METHODS:
      constructor IMPORTING is_structure   TYPE sstruc
                            iv_id          TYPE i
                            io_scan_result TYPE REF TO zcl_ppr_scan_result,
      get_structure_type RETURNING VALUE(rv_type) TYPE stru_type,
      get_structure_type_text RETURNING VALUE(rv_text) TYPE string,
      has_special_start_statement RETURNING VALUE(rv_true) TYPE abap_bool,
      has_special_end_statement RETURNING VALUE(rv_true) TYPE abap_bool,
      get_statements RETURNING VALUE(rt_statements) TYPE zcl_ppr_scan_result=>gty_statement_object_tab,
      get_statement IMPORTING iv_index            TYPE i
                    RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_scan_statement,
      get_sub_structures RETURNING VALUE(rt_structures) TYPE zcl_ppr_scan_result=>gty_structure_object_tab,
      get_all_sub_structures RETURNING VALUE(rt_structures) TYPE zcl_ppr_scan_result=>gty_structure_object_tab,
      get_parent_structure RETURNING VALUE(ro_parent) TYPE REF TO zcl_ppr_scan_structure,
      has_parent_structure RETURNING VALUE(rv_true) TYPE abap_bool,
      is_descendant_of_mine IMPORTING io_candidate   TYPE REF TO zcl_ppr_scan_structure
                            RETURNING VALUE(rv_true) TYPE abap_bool.
    DATA:
      ms_structure TYPE sstruc READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_scan_structure IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_id               = iv_id
                        io_scan_result      = io_scan_result
                        iv_result_type_name = gc_result_type_name ).
    ms_structure = is_structure.
    mv_id = iv_id.
    mo_scan_result = io_scan_result.
  ENDMETHOD.

  METHOD get_parent_structure.
    IF has_parent_structure( ) = abap_false.
      ##TODO.
    ENDIF.

    ro_parent = mo_scan_result->get_structure_by_id( ms_structure-back ).
  ENDMETHOD.

  METHOD get_statements.
    DATA(lv_from) = COND #( WHEN ms_structure-stmnt_from < ms_structure-stmnt_to
                            THEN ms_structure-stmnt_from ELSE ms_structure-stmnt_to ).
    DATA(lv_to) = COND #( WHEN ms_structure-stmnt_to > ms_structure-stmnt_from
                          THEN ms_structure-stmnt_to ELSE ms_structure-stmnt_from ).
    DO lv_to - lv_from + 1 TIMES.
      APPEND mo_scan_result->get_statement_by_id( lv_from + sy-index - 1 ) TO rt_statements.
    ENDDO.
  ENDMETHOD.

  METHOD get_statement.
    DATA(lt_statements) = get_statements( ).
    ro_statement = lt_statements[ iv_index ].
  ENDMETHOD.

  METHOD get_structure_type.
    rv_type = ms_structure-type.
  ENDMETHOD.

  METHOD get_structure_type_text.
    CONSTANTS: lc_structure_types LIKE zcl_ppr_constants=>gc_scan_struc_types
                                  VALUE zcl_ppr_constants=>gc_scan_struc_types.

    rv_text = SWITCH #( get_structure_type( )
      WHEN lc_structure_types-prog        THEN 'Source Code Begin'
      WHEN lc_structure_types-routine     THEN 'FORM/FUNCTION/MODULE'
      WHEN lc_structure_types-macro       THEN 'MAKRO/EXEC SQL'
      WHEN lc_structure_types-iteration   THEN 'Loops'
      WHEN lc_structure_types-alternation THEN 'IF/WHEN'
      WHEN lc_structure_types-condition   THEN 'condition in ALTERNATION'
      WHEN lc_structure_types-jump        THEN 'check...'
      WHEN lc_structure_types-declaration THEN 'structured declaration'
      WHEN lc_structure_types-event       THEN 'events'
      WHEN lc_structure_types-sequence    THEN 'statement sequence'
      WHEN lc_structure_types-class       THEN 'oo structuring'
      ELSE 'Unknown'
    ) ##NO_TEXT.
  ENDMETHOD.

  METHOD get_sub_structures.
    DO ms_structure-struc_to - ms_structure-struc_from + 1 TIMES.
      APPEND mo_scan_result->get_structure_by_id( ms_structure-struc_from + sy-index - 1 ) TO rt_structures.
    ENDDO.
  ENDMETHOD.

  METHOD get_all_sub_structures.
    LOOP AT get_sub_structures( ) INTO DATA(lo_child).
      APPEND lo_child TO rt_structures.
      APPEND LINES OF lo_child->get_all_sub_structures( ) TO rt_structures.
    ENDLOOP.
  ENDMETHOD.

  METHOD has_parent_structure.
    rv_true = boolc( ms_structure-back IS NOT INITIAL ).
  ENDMETHOD.

  METHOD has_special_end_statement.
    rv_true = ms_structure-key_start.
  ENDMETHOD.

  METHOD has_special_start_statement.
    rv_true = ms_structure-key_end.
  ENDMETHOD.

  METHOD is_descendant_of_mine.
    DATA(lt_sub_structures) = get_all_sub_structures( ).
    rv_true = boolc( line_exists( lt_sub_structures[ table_line = io_candidate ] ) ).
  ENDMETHOD.
ENDCLASS.
