"! Scanned Token
CLASS zcl_ppr_scan_token DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_ppr_scan_return_value_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      gc_result_type_name TYPE string VALUE 'TOKEN'.
    METHODS:
      constructor IMPORTING is_token       TYPE stokesx
                            iv_id          TYPE i
                            io_scan_result TYPE REF TO zcl_ppr_scan_result,
      get_statement RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_scan_statement,
      get_structure RETURNING VALUE(ro_structure) TYPE REF TO zcl_ppr_scan_structure,
      get_description REDEFINITION,
      get_token_text RETURNING VALUE(rv_text) TYPE string,
      get_row RETURNING VALUE(rv_row) TYPE token_row ,
      get_horizontal_offset RETURNING VALUE(rv_offset) TYPE token_col,
      get_token_type RETURNING VALUE(rv_type) TYPE token_type,
      get_token_type_text RETURNING VALUE(rv_text) TYPE string.
    DATA:
      ms_token TYPE stokesx READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_scan_token IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_id               = iv_id
                        iv_result_type_name = gc_result_type_name
                        io_scan_result      = io_scan_result ).
    ms_token = is_token.
    mv_id = iv_id.
    mo_scan_result = mo_scan_result.
  ENDMETHOD.

  METHOD get_statement.
    ro_statement = mo_scan_result->get_statement_by_token( me ).
  ENDMETHOD.

  METHOD get_structure.
    ro_structure = mo_scan_result->get_structure_by_id( ms_token-row ).
  ENDMETHOD.

  METHOD get_description.
    rv_description = |{ super->get_description( ) }: { get_token_text( ) }|.
  ENDMETHOD.

  METHOD get_token_text.
    rv_text = ms_token-str.
  ENDMETHOD.

  METHOD get_horizontal_offset.
    rv_offset = ms_token-col.
  ENDMETHOD.

  METHOD get_row.
    rv_row = ms_token-row.
  ENDMETHOD.

  METHOD get_token_type.
    rv_type = ms_token-type.
  ENDMETHOD.

  METHOD get_token_type_text.
    CONSTANTS: lc_token_types LIKE zcl_ppr_constants=>gc_scan_token_types
                              VALUE zcl_ppr_constants=>gc_scan_token_types.

    rv_text = SWITCH #( get_token_type( )
      WHEN lc_token_types-identifier THEN 'Identifier'
      WHEN lc_token_types-literal    THEN 'Literal'
      WHEN lc_token_types-list       THEN 'List'
      WHEN lc_token_types-comment    THEN 'Comment'
      WHEN lc_token_types-list_begin THEN 'List Begin'
      WHEN lc_token_types-list_sep   THEN 'List Separator'
      WHEN lc_token_types-list_end   THEN 'List End'
      WHEN lc_token_types-pragma     THEN 'Pragma'
      ELSE 'Unknown'
    ) ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.
