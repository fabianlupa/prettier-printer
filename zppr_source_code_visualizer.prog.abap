REPORT zppr_source_code_visualizer NO STANDARD PAGE HEADING.

PARAMETERS: p_objt   TYPE trobjtype OBLIGATORY DEFAULT 'PROG',
            p_objn   TYPE sobj_name OBLIGATORY DEFAULT 'ZPPR_SOURCE_CODE_VISUALIZER',
            p_active TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X',
            p_inacti TYPE abap_bool RADIOBUTTON GROUP r1.

CLASS lcx_error DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING ix_previous TYPE REF TO cx_root OPTIONAL
                            iv_reason   TYPE csequence,
      get_text REDEFINITION.
    DATA:
      mv_reason TYPE string READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcx_error IMPLEMENTATION.
  METHOD constructor.
    super->constructor( previous = ix_previous ).
    mv_reason = iv_reason.
  ENDMETHOD.

  METHOD get_text.
    result = mv_reason.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    METHODS:
      run RAISING lcx_error,
      analyze_line IMPORTING iv_line TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF gty_token.
        INCLUDE TYPE stokesx.
    TYPES: typetxt TYPE string,
           END OF gty_token,
           BEGIN OF gty_statement.
        INCLUDE TYPE sstmnt.
    TYPES: typetxt TYPE string,
           END OF gty_statement,
           BEGIN OF gty_structure.
        INCLUDE TYPE sstruc.
    TYPES: typetxt      TYPE string,
           stmnttypetxt TYPE string,
           END OF gty_structure.
    CLASS-METHODS:
      prepare_token_for_output IMPORTING is_token        TYPE stokesx
                               RETURNING VALUE(rs_token) TYPE gty_token,
      prepare_statement_for_output IMPORTING is_statement        TYPE sstmnt
                                   RETURNING VALUE(rs_statement) TYPE gty_statement,
      prepare_structure_for_output IMPORTING is_structure        TYPE sstruc
                                   RETURNING VALUE(rs_structure) TYPE gty_structure.
    METHODS:
      read_source RAISING lcx_error,
      display_as_list,
      scan_source,
      format_output_tables.
    DATA:
      mt_source         TYPE stringtab,
      mt_tokens         TYPE stokesx_tab,
      mt_tokens_out     TYPE STANDARD TABLE OF gty_token,
      mt_statements     TYPE sstmnt_tab,
      mt_statements_out TYPE STANDARD TABLE OF gty_statement,
      mt_structures     TYPE sstruc_tab,
      mt_structures_out TYPE STANDARD TABLE OF gty_structure.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD run.
    read_source( ).
    scan_source( ).
    format_output_tables( ).
    display_as_list( ).
  ENDMETHOD.

  METHOD analyze_line.
    DATA: lv_min_token TYPE i,
          lv_max_token TYPE i,
          lv_tabix     TYPE syst_tabix.

    DATA(li_output) = cl_demo_output=>new( ).

    li_output->begin_section( 'Tokens' ).
    LOOP AT mt_tokens REFERENCE INTO DATA(lr_token) WHERE row = iv_line.
      lv_tabix = sy-tabix.

      IF lv_min_token IS INITIAL.
        lv_min_token = lv_tabix.
      ENDIF.
      li_output->write( name = |{ lr_token->str } { lv_tabix }|
                        data = prepare_token_for_output( lr_token->* ) ).
    ENDLOOP.
    lv_max_token = lv_tabix.
    li_output->end_section( ).

    li_output->begin_section( 'Structure' ).
    DATA(lr_structure) = REF #( mt_structures[ iv_line ] ).
    li_output->write( name = |{ line_index( mt_structures[ table_line = lr_structure->* ] ) }|
                      data = prepare_structure_for_output( lr_structure->* ) ).
    li_output->end_section( ).

    li_output->begin_section( 'Statements' ).
    LOOP AT mt_statements REFERENCE INTO DATA(lr_statement)
                          WHERE from BETWEEN lv_min_token AND lv_max_token.
      li_output->write( name = |{ lr_statement->type } { sy-index }|
                        data = prepare_statement_for_output( lr_statement->* ) ).
    ENDLOOP.
    li_output->end_section( ).

    li_output->display( ).
  ENDMETHOD.

  METHOD read_source.
    DATA(lv_state) = COND #( WHEN p_active = abap_true THEN 'A' ELSE 'I' ).

    IF p_objt <> 'PROG'.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          iv_reason = |Object type '{ p_objt }' not supported.|.
    ENDIF.

    READ REPORT p_objn INTO mt_source STATE lv_state.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          iv_reason = |Could not read source code of program { p_objn }.|.
    ENDIF.

    IF mt_source IS INITIAL.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          iv_reason = |Source code is empty.|.
    ENDIF.
  ENDMETHOD.

  METHOD scan_source.
    SCAN ABAP-SOURCE mt_source
      TOKENS INTO mt_tokens
      STATEMENTS INTO mt_statements
      STRUCTURES INTO mt_structures
      WITH ANALYSIS
      WITH COMMENTS
      WITH BLOCKS
      WITH DECLARATIONS
      WITH PRAGMAS '*'
      PRESERVING IDENTIFIER ESCAPING
      WITHOUT TRMAC.
  ENDMETHOD.

  METHOD format_output_tables.
    CLEAR: mt_statements_out,
           mt_structures_out,
           mt_tokens_out.

    LOOP AT mt_statements ASSIGNING FIELD-SYMBOL(<ls_statement>).
      APPEND prepare_statement_for_output( <ls_statement> ) TO mt_statements_out.
    ENDLOOP.

    LOOP AT mt_structures ASSIGNING FIELD-SYMBOL(<ls_structure>).
      APPEND prepare_structure_for_output( <ls_structure> ) TO mt_structures_out.
    ENDLOOP.

    LOOP AT mt_tokens ASSIGNING FIELD-SYMBOL(<ls_token>).
      APPEND prepare_token_for_output( <ls_token> ) TO mt_tokens_out.
    ENDLOOP.
  ENDMETHOD.

  METHOD display_as_list.
    LOOP AT mt_source ASSIGNING FIELD-SYMBOL(<lv_line>).
      IF <lv_line> IS NOT INITIAL.
        WRITE: / <lv_line>.
      ELSE.
        SKIP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD prepare_statement_for_output.
    CONSTANTS: lc_stmnt_types LIKE zcl_ppr_constants=>gc_scan_stmnt_types
                              VALUE zcl_ppr_constants=>gc_scan_stmnt_types.

    rs_statement = CORRESPONDING #( is_statement ).
    rs_statement-typetxt = SWITCH #( rs_statement-type
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
    ).
  ENDMETHOD.

  METHOD prepare_structure_for_output.
    CONSTANTS: lc_struc_types LIKE zcl_ppr_constants=>gc_scan_struc_types
                              VALUE zcl_ppr_constants=>gc_scan_struc_types.

    rs_structure = CORRESPONDING #( is_structure ).
    rs_structure-typetxt = SWITCH #( is_structure-type
      WHEN lc_struc_types-alternation THEN 'Alternation'
      WHEN lc_struc_types-class       THEN 'Class'
      WHEN lc_struc_types-condition   THEN 'Condition'
      WHEN lc_struc_types-declaration THEN 'Declaration'
      WHEN lc_struc_types-event       THEN 'Event'
      WHEN lc_struc_types-iteration   THEN 'Iteration'
      WHEN lc_struc_types-jump        THEN 'Jump'
      WHEN lc_struc_types-macro       THEN 'Macro'
      WHEN lc_struc_types-prog        THEN 'Prog'
      WHEN lc_struc_types-routine     THEN 'Routine'
      WHEN lc_struc_types-sequence    THEN 'Sequence'
    ).
  ENDMETHOD.

  METHOD prepare_token_for_output.
    rs_token = CORRESPONDING #( is_token ).
  ENDMETHOD.
ENDCLASS.

DATA: go_main TYPE REF TO lcl_main.

START-OF-SELECTION.
  TRY.
      go_main = NEW lcl_main( ).
      go_main->run( ).
    CATCH lcx_error INTO DATA(gx_ex).
      MESSAGE gx_ex TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
  ENDTRY.

AT LINE-SELECTION.
  go_main->analyze_line( sy-lilli ).
