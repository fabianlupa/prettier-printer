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
      append_structure_nodes_rec IMPORTING io_nodes            TYPE REF TO cl_salv_nodes
                                           iv_parent_node_key  TYPE salv_de_node_key
                                           io_parent_structure TYPE REF TO zcl_ppr_scan_structure.
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
          DATA(lo_statement_node) = mo_tree->get_nodes( )->add_node(
            related_node   = lo_statement_root->get_key( )
            relationship   = if_salv_c_node_relation=>last_child
            text           = CONV #( lo_statement->get_statement_text( ) )
            data_row       = scan_return_value_to_tree_data( lo_statement )
          ).

          DATA(lo_statement_tokens_node) = mo_tree->get_nodes( )->add_node(
            related_node   = lo_statement_node->get_key( )
            relationship   = if_salv_c_node_relation=>last_child
            text           = 'Tokens'
            folder         = abap_true
          ).
          LOOP AT lo_statement->get_tokens( ) INTO DATA(lo_token).
            mo_tree->get_nodes( )->add_node(
              related_node   = lo_statement_tokens_node->get_key( )
              relationship   = if_salv_c_node_relation=>last_child
              data_row       = scan_return_value_to_tree_data( lo_token )
              text           = CONV #( lo_token->get_token_text( ) )
            ).
          ENDLOOP.

          DATA(lo_structure) = lo_statement->get_structure( ).
          DATA(lo_statement_structure_node) = mo_tree->get_nodes( )->add_node(
            related_node   = lo_statement_node->get_key( )
            relationship   = if_salv_c_node_relation=>last_child
            data_row       = scan_return_value_to_tree_data( lo_structure )
            text           = CONV #( lo_structure->get_description( ) )
          ).
        ENDLOOP.

        LOOP AT mo_result->mt_structures INTO DATA(lo_structure2).
          IF lo_structure2->has_parent_structure( ) = abap_true.
            CONTINUE.
          ENDIF.

          DATA(lo_structure_node) = mo_tree->get_nodes( )->add_node(
            related_node   = lo_structure_root->get_key( )
            relationship   = if_salv_c_node_relation=>last_child
            text           = CONV #( lo_structure2->get_structure_type_text( ) )
            data_row       = scan_return_value_to_tree_data( lo_structure2 )
          ).

          append_structure_nodes_rec( io_nodes            = mo_tree->get_nodes( )
                                      iv_parent_node_key  = lo_structure_node->get_key( )
                                      io_parent_structure = lo_structure2 ).
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
      )->write_text( lo_value->get_description( )
      )->write( data = lo_value->mv_id name = 'ID'
      )->write( data = lo_value->mv_type_name name = 'TYPE'
      )->begin_section( 'Data' ).

    CASE lo_value->mv_type_name.
      WHEN zcl_ppr_scan_statement=>gc_result_type_name.
        DATA(lo_statement) = CAST zcl_ppr_scan_statement( lo_value ).
        li_output->write_data( lo_statement->ms_statement ).

      WHEN zcl_ppr_scan_token=>gc_result_type_name.
        DATA(lo_token) = CAST zcl_ppr_scan_token( lo_value ).
        li_output->write_data( lo_token->ms_token ).

      WHEN zcl_ppr_scan_structure=>gc_result_type_name.
        DATA(lo_structure) = CAST zcl_ppr_scan_structure( lo_value ).
        li_output->write_data( lo_structure->ms_structure ).

    ENDCASE.

    li_output->end_section( )->display( ).
  ENDMETHOD.

  METHOD scan_return_value_to_tree_data.
    rs_data = VALUE gty_tree_data( text = io_value->get_description( )
                                   type = io_value->mv_type_name
                                   id   = io_value->mv_id ).
  ENDMETHOD.

  METHOD append_structure_nodes_rec.
    LOOP AT io_parent_structure->get_sub_structures( ) INTO DATA(lo_structure).
      DATA(lo_structure_node) = io_nodes->add_node(
        related_node   = iv_parent_node_key
        relationship   = if_salv_c_node_relation=>last_child
        text           = CONV #( lo_structure->get_structure_type_text( ) )
        data_row       = scan_return_value_to_tree_data( lo_structure )
      ).
      append_structure_nodes_rec( io_nodes            = io_nodes
                                  iv_parent_node_key  = lo_structure_node->get_key( )
                                  io_parent_structure = lo_structure ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  TRY.
      NEW lcl_main( )->run( ).
    CATCH lcx_error INTO DATA(gx_ex).
      MESSAGE gx_ex TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
  ENDTRY.
