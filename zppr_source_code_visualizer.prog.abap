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
    METHODS:
      read_source RAISING lcx_error,
      display_as_list,
      scan_source.
    DATA:
      mt_source     TYPE stringtab,
      mt_tokens     TYPE stokesx_tab,
      mt_statements TYPE sstmnt_tab,
      mt_structures TYPE sstruc_tab.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD run.
    read_source( ).
    scan_source( ).
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
      li_output->write( name = lr_token->str data = lr_token->* ).
    ENDLOOP.
    lv_max_token = lv_tabix.
    li_output->end_section( ).

    li_output->begin_section( 'Structure' ).
    DATA(lr_structure) = REF #( mt_structures[ iv_line ] ).
    li_output->write( lr_structure->* ).
    li_output->end_section( ).

    li_output->begin_section( 'Statements' ).
    LOOP AT mt_statements REFERENCE INTO DATA(lr_statement)
                          WHERE from BETWEEN lv_min_token AND lv_max_token.
      li_output->write( name = |{ lr_statement->type }| data = lr_statement->* ).
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

  METHOD display_as_list.
    LOOP AT mt_source ASSIGNING FIELD-SYMBOL(<lv_line>).
      IF <lv_line> IS NOT INITIAL.
        WRITE: / <lv_line>.
      ELSE.
        SKIP.
      ENDIF.
    ENDLOOP.
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
