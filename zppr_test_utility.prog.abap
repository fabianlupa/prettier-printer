REPORT zppr_test_utility.

PARAMETERS: p_dummy ##NEEDED.

CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor.
    METHODS:
      run.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF gc_fc,
        run   TYPE ui_func VALUE 'RUN',
        debug TYPE ui_func VALUE 'DEBUG',
        copy  TYPE ui_func VALUE 'COPY',
      END OF gc_fc.
    METHODS:
      on_function_selected FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode,
      format_source IMPORTING iv_debug TYPE abap_bool.
    CLASS-DATA:
      gt_dummy_code TYPE stringtab.
    DATA:
      mo_splitter     TYPE REF TO cl_gui_splitter_container,
      mo_editor_left  TYPE REF TO cl_gui_abapedit,
      mo_editor_right TYPE REF TO cl_gui_abapedit,
      mo_toolbar      TYPE REF TO cl_gui_toolbar.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD class_constructor.
    gt_dummy_code = VALUE #(
      ( `CLASS lcl_test DEFINITION.` )
      ( `PUBLIC SECTION.` )
      ( `   METHODS:`)
      ( `     do_stuff IMPORTING iv_param TYPE clike.` )
      ( `  PRIVATE SECTION.`)
      ( `  ENDCLASS.` )
      ( `` )
      ( `` )
      ( `CLASS lcl_test      IMPLEMENTATION   .` )
      ( `  METHOD do_stuff.` )
      ( `    IF 1 = 2.` )
      ( `      DATA(lv_string) = |This is a text that spans across| &&` )
      ( `                | multiple lines.|.` )
      ( `    ENDIF.` )
      ( `  ENDMETHOD.` )
      ( `ENDCLASS.` )
    ).
  ENDMETHOD.

  METHOD run.
    DATA: lt_dummy TYPE stringtab.

    IF mo_splitter IS NOT BOUND.
      mo_splitter = NEW #( parent = cl_gui_container=>screen0 columns = 3 rows = 1 ).
      mo_splitter->set_column_mode( cl_gui_splitter_container=>mode_absolute ).
      mo_splitter->set_column_width( id = 2 width = 26 ).
      mo_splitter->set_column_sash( id = 1 type = cl_gui_splitter_container=>type_sashvisible value = 0 ).
      mo_splitter->set_column_sash( id = 1 type = cl_gui_splitter_container=>type_sashvisible value = 0 ).
      mo_splitter->set_column_sash( id = 2 type = cl_gui_splitter_container=>type_movable value = 0 ).
      mo_splitter->set_column_sash( id = 2 type = cl_gui_splitter_container=>type_movable value = 0 ).
      mo_editor_left = NEW #( parent = mo_splitter->get_container( row = 1 column = 1 ) ).
      mo_editor_left->set_text( gt_dummy_code ).
      mo_editor_right = NEW #( parent = mo_splitter->get_container( row = 1 column = 3 ) ).
      mo_editor_right->set_readonly_mode( ).
      mo_toolbar = NEW #( parent       = mo_splitter->get_container( row = 1 column = 2 )
                          display_mode = cl_gui_toolbar=>m_mode_vertical ).
      mo_toolbar->set_alignment( cl_gui_toolbar=>align_at_left ).
      mo_toolbar->set_registered_events( VALUE #( ( eventid = cl_gui_toolbar=>m_id_function_selected ) ) ).
      SET HANDLER on_function_selected FOR mo_toolbar.
      mo_toolbar->add_button_group( VALUE #(
        ( function = gc_fc-run   icon = icon_arrow_right quickinfo = 'Run' )
        ( function = gc_fc-debug icon = icon_debugger_step_into quickinfo = 'Debug' )
        ( function = gc_fc-copy icon = icon_arrow_left quickinfo = 'Replace with last result' )
      ) ).
    ENDIF.
  ENDMETHOD.

  METHOD on_function_selected.
    DATA: lt_formatted TYPE stringtab.

    CASE fcode.
      WHEN gc_fc-run.
        format_source( abap_false ).
      WHEN gc_fc-debug.
        format_source( abap_true ).
      WHEN gc_fc-copy.
        mo_editor_right->get_text(
          IMPORTING
            table                  = lt_formatted
          EXCEPTIONS
            error_dp               = 1
            error_cntl_call_method = 2
            others                 = 3
        ).
        IF sy-subrc <> 0.
          " Error is thrown if the control is empty
        ENDIF.
        mo_editor_left->set_text( lt_formatted ).
    ENDCASE.
  ENDMETHOD.

  METHOD format_source.
    DATA: lt_source TYPE stringtab.

    mo_editor_left->draw( ).
    mo_editor_left->get_text( IMPORTING table = lt_source ).
    DATA(lo_formatter) = NEW zcl_ppr_formatter( ).
    DATA(lo_config) = NEW zcl_ppr_configuration( ).

    IF iv_debug = abap_true.
      BREAK-POINT.
    ENDIF.

    DATA(lt_formatted) = lo_formatter->format_source(
      it_source        = lt_source
      io_configuration = lo_config
    ).

    mo_editor_right->set_text( lt_formatted ).
  ENDMETHOD.
ENDCLASS.

DATA: go_main TYPE REF TO lcl_main.

AT SELECTION-SCREEN OUTPUT.
  IF go_main IS NOT BOUND.
    go_main = NEW #( ).
  ENDIF.

  go_main->run( ).
