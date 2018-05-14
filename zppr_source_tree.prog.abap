REPORT zppr_source_tree.

PARAMETERS: p_objt   TYPE trobjtype OBLIGATORY DEFAULT 'PROG',
            p_objn   TYPE sobj_name OBLIGATORY DEFAULT 'ZPPR_SOURCE_TREE',
            p_active TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X',
            p_inacti TYPE abap_bool RADIOBUTTON GROUP r1.

CLASS lcx_error DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_text     TYPE csequence OPTIONAL
                            ix_previous TYPE REF TO cx_root OPTIONAL
                              PREFERRED PARAMETER iv_text,
      get_text REDEFINITION.
    DATA:
      mv_text TYPE string READ-ONLY.
ENDCLASS.

CLASS lcx_error IMPLEMENTATION.
  METHOD constructor.
    super->constructor( previous = ix_previous ).
    mv_text = iv_text.
  ENDMETHOD.

  METHOD get_text.
    result = mv_text.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,
      run RAISING lcx_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF gty_tree_data,
        text TYPE string,
        type TYPE string,
        id   TYPE i,
      END OF gty_tree_data.
    CLASS-METHODS:
      scan_return_value_to_tree_data IMPORTING io_value       TYPE REF TO zcl_ppr_scan_return_value_base
                                     RETURNING VALUE(rs_data) TYPE gty_tree_data,
      append_structure_nodes_rec IMPORTING io_nodes             TYPE REF TO cl_salv_nodes
                                           iv_parent_node_key   TYPE salv_de_node_key
                                           io_structure         TYPE REF TO zcl_ppr_scan_structure
                                           iv_append_statements TYPE abap_bool
                                           iv_append_tokens     TYPE abap_bool,
      append_statement_node IMPORTING io_nodes           TYPE REF TO cl_salv_nodes
                                      iv_parent_node_key TYPE salv_de_node_key
                                      io_statement       TYPE REF TO zcl_ppr_scan_statement
                                      iv_append_tokens   TYPE abap_bool
                                      iv_show_structure  TYPE abap_bool.
    METHODS:
      read_source RAISING lcx_error,
      analyze RAISING lcx_error,
      display_tree RAISING lcx_error,
      on_double_click FOR EVENT double_click OF cl_salv_events_tree IMPORTING node_key.
    DATA:
      mo_scanner   TYPE REF TO zcl_ppr_oo_scanner,
      mo_result    TYPE REF TO zcl_ppr_scan_result,
      mt_source    TYPE stringtab,
      mt_tree_data TYPE STANDARD TABLE OF gty_tree_data,
      mo_tree      TYPE REF TO cl_salv_tree.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD constructor.
    mo_scanner = NEW #( ).
  ENDMETHOD.

  METHOD run.
    read_source( ).
    analyze( ).
    display_tree( ).
  ENDMETHOD.

  METHOD analyze.
    mo_result = mo_scanner->scan_source( mt_source ).
  ENDMETHOD.

  METHOD display_tree.
    IF mo_tree IS NOT BOUND.
      TRY.
          cl_salv_tree=>factory(
            IMPORTING
              r_salv_tree   = mo_tree
            CHANGING
              t_table       = mt_tree_data
          ).
          SET HANDLER on_double_click FOR mo_tree->get_event( ).

        CATCH cx_salv_error INTO DATA(lx_ex).
          RAISE EXCEPTION TYPE lcx_error
            EXPORTING
              iv_text     = lx_ex->get_text( )
              ix_previous = lx_ex.
      ENDTRY.
    ENDIF.

    TRY.
        mo_tree->get_nodes( )->delete_all( ).
        DATA(lo_root) = mo_tree->get_nodes( )->add_node(
          related_node   = space
          relationship   = if_salv_c_node_relation=>last_child
          text           = 'Root'
        ).
        DATA(lo_statement_root) = mo_tree->get_nodes( )->add_node(
          related_node   = lo_root->get_key( )
          relationship   = if_salv_c_node_relation=>last_child
          text           = 'Statements'
        ).
        DATA(lo_structure_root) = mo_tree->get_nodes( )->add_node(
          related_node   = lo_root->get_key( )
          relationship   = if_salv_c_node_relation=>last_child
          text           = 'Structures'
        ).

        LOOP AT mo_result->mt_statements INTO DATA(lo_statement).
          append_statement_node( io_nodes           = mo_tree->get_nodes( )
                                 iv_parent_node_key = lo_statement_root->get_key( )
                                 io_statement       = lo_statement
                                 iv_append_tokens   = abap_true
                                 iv_show_structure  = abap_true ).
        ENDLOOP.

        LOOP AT mo_result->mt_structures INTO DATA(lo_structure2).
          IF lo_structure2->has_parent_structure( ) = abap_true.
            CONTINUE.
          ENDIF.

          append_structure_nodes_rec( io_nodes             = mo_tree->get_nodes( )
                                      iv_parent_node_key   = lo_structure_root->get_key( )
                                      io_structure         = lo_structure2
                                      iv_append_statements = abap_true
                                      iv_append_tokens     = abap_true ).
        ENDLOOP.

        lo_root->expand( ).
        mo_tree->get_columns( )->set_optimize( ).
        mo_tree->display( ).

      CATCH cx_salv_msg cx_salv_error INTO DATA(lx_ex2).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            iv_text     = lx_ex2->get_text( )
            ix_previous = lx_ex2.
    ENDTRY.
  ENDMETHOD.

  METHOD read_source.
    DATA(lv_state) = COND #( WHEN p_active = abap_true THEN 'A' ELSE 'I' ).

    IF p_objt <> 'PROG'.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          iv_text = |Object type '{ p_objt }' not supported.|.
    ENDIF.

    READ REPORT p_objn INTO mt_source STATE lv_state.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          iv_text = |Could not read source code of program { p_objn }.|.
    ENDIF.

    IF mt_source IS INITIAL.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          iv_text = |Source code is empty.|.
    ENDIF.
  ENDMETHOD.

  METHOD on_double_click.
    CHECK node_key IS NOT INITIAL.
    DATA(lr_data) = CAST gty_tree_data( mo_tree->get_nodes( )->get_node( node_key )->get_data_row( ) ).
    CHECK lr_data->type IS NOT INITIAL AND lr_data->id IS NOT INITIAL.
    DATA(lo_value) = mo_result->get_return_value_generic( iv_type = lr_data->type iv_id = lr_data->id ).

    DATA(li_output) = cl_demo_output=>new(
      )->write_data( value = lo_value->get_description( ) name = 'Description'
      )->write_data( value = lo_value->mv_id name = 'ID'
      )->write_data( value = lo_value->mv_type_name name = 'TYPE'
      )->begin_section( 'Data' ).

    CASE lo_value->mv_type_name.
      WHEN zcl_ppr_scan_statement=>gc_result_type_name.
        DATA(lo_statement) = CAST zcl_ppr_scan_statement( lo_value ).
        li_output->write_data( lo_statement->ms_statement
          )->write_data( value = lo_statement->is_statement_first_in_line( ) name = 'First in line'
          )->write_data( value = lo_statement->is_part_of_chained_statement( ) name = 'Part of chain'
          )->write_data( value = lo_statement->get_first_line_number( ) name = 'First line'
          )->write_data( value = lo_statement->get_last_line_number( ) name = 'Last line'
          )->write_data( value = lo_statement->get_source( ) name = 'Source (might include other statements)'
          )->write_data( value = lo_statement->get_statement_text( ) name = 'Tokens in statement'
          )->write_data( value = CONV i( lines( lo_statement->get_tokens( ) ) ) name = 'Tokens count'
          )->write_data( value = lo_statement->get_statement_type_name( ) name = 'Statement type name' ).

      WHEN zcl_ppr_scan_token=>gc_result_type_name.
        DATA(lo_token) = CAST zcl_ppr_scan_token( lo_value ).
        li_output->write_data( lo_token->ms_token
          )->write_data( value = lo_token->get_horizontal_offset( ) name = 'Horizontal offset'
          )->write_data( value = lo_token->get_row( ) name = 'Row'
          )->write_data( value = lo_token->get_token_text( ) name = 'Token text'
          )->write_data( value = lo_token->get_token_type_text( ) name = 'Token type text' ).

      WHEN zcl_ppr_scan_structure=>gc_result_type_name.
        DATA(lo_structure) = CAST zcl_ppr_scan_structure( lo_value ).
        li_output->write_data( lo_structure->ms_structure
          )->write_data( value = lo_structure->get_structure_type_text( ) name = 'Structure type text'
          )->write_data( value = lo_structure->has_parent_structure( ) name = 'Has parent structure'
          )->write_data( value = CONV i( lines( lo_structure->get_sub_structures( ) ) ) name = 'Children count'
          )->write_data( value = lo_structure->has_special_start_statement( ) name = 'Has special start statement'
          )->write_data( value = lo_structure->has_special_end_statement( ) name = 'Has special end statement' ).

    ENDCASE.

    li_output->end_section( )->display( ).
  ENDMETHOD.

  METHOD scan_return_value_to_tree_data.
    rs_data = VALUE gty_tree_data( text = io_value->get_description( )
                                   type = io_value->mv_type_name
                                   id   = io_value->mv_id ).
  ENDMETHOD.

  METHOD append_structure_nodes_rec.
    DATA(lo_structure_node) = io_nodes->add_node(
      related_node   = iv_parent_node_key
      relationship   = if_salv_c_node_relation=>last_child
      text           = CONV #( io_structure->get_structure_type_text( ) )
      data_row       = scan_return_value_to_tree_data( io_structure )
      collapsed_icon = CONV #( icon_structure )
      expanded_icon  = CONV #( icon_structure )
    ).

    IF iv_append_statements = abap_true. "AND lines( io_structure->get_sub_structures( ) ) = 0.
      LOOP AT io_structure->get_statements( ) INTO DATA(lo_statement).
        append_statement_node( io_nodes           = io_nodes
                               iv_parent_node_key = lo_structure_node->get_key( )
                               iv_show_structure  = abap_false
                               iv_append_tokens   = abap_true
                               io_statement       = lo_statement ).
      ENDLOOP.
    ENDIF.

    LOOP AT io_structure->get_sub_structures( ) INTO DATA(lo_structure).
      append_structure_nodes_rec( io_nodes             = io_nodes
                                  iv_parent_node_key   = lo_structure_node->get_key( )
                                  io_structure         = lo_structure
                                  iv_append_statements = iv_append_statements
                                  iv_append_tokens     = iv_append_tokens ).
    ENDLOOP.
  ENDMETHOD.

  METHOD append_statement_node.
    DATA(lo_statement_node) = io_nodes->add_node(
      related_node   = iv_parent_node_key
      relationship   = if_salv_c_node_relation=>last_child
      text           = CONV #( io_statement->get_statement_text( ) )
      data_row       = scan_return_value_to_tree_data( io_statement )
      collapsed_icon = CONV #( icon_oo_method )
      expanded_icon  = CONV #( icon_oo_method )
    ).

    IF iv_append_tokens = abap_true.
      LOOP AT io_statement->get_tokens( ) INTO DATA(lo_token).
        io_nodes->add_node(
          related_node   = lo_statement_node->get_key( )
          relationship   = if_salv_c_node_relation=>last_child
          data_row       = scan_return_value_to_tree_data( lo_token )
          text           = CONV #( lo_token->get_token_text( ) )
          collapsed_icon = CONV #( icon_abc )
          expanded_icon  = CONV #( icon_abc )
        ).
      ENDLOOP.
    ENDIF.

    IF iv_show_structure = abap_true.
      append_structure_nodes_rec( io_nodes             = io_nodes
                                  iv_parent_node_key   = lo_statement_node->get_key( )
                                  io_structure         = io_statement->get_structure( )
                                  iv_append_statements = abap_false
                                  iv_append_tokens     = abap_false ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  TRY.
      NEW lcl_main( )->run( ).
    CATCH lcx_error INTO DATA(gx_ex).
      MESSAGE gx_ex TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
  ENDTRY.
