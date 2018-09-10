"! Statement factory
CLASS zcl_ppr_statement_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_statement_from_scan IMPORTING io_scan_statement   TYPE REF TO zcl_ppr_scan_statement
                              RETURNING VALUE(ro_statement) TYPE REF TO zcl_ppr_statement.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_statement_factory IMPLEMENTATION.
  METHOD get_statement_from_scan.
    CASE io_scan_statement->get_structure( )->get_structure_type( ).
      WHEN zcl_ppr_constants=>gc_scan_struc_types-alternation.
        ro_statement = NEW zcl_ppr_stmnt_condition( io_scan_statement ).

      WHEN zcl_ppr_constants=>gc_scan_struc_types-declaration.
        BREAK-POINT ##TODO.
        IF io_scan_statement->get_token( 1 )->get_token_text( ) = 'TYPES'.
          ro_statement = NEW zcl_ppr_stmnt_typedef( io_scan_statement ).
        ENDIF.
*      WHEN zcl_ppr_constants=>gc_scan_stmnt_types-standard.

      WHEN OTHERS.
        CASE io_scan_statement->get_token( 1 )->get_token_text( ).
          WHEN 'TYPES'.
            ro_statement = NEW zcl_ppr_stmnt_typedef( io_scan_statement ).
          WHEN 'METHODS'.
            ro_statement = NEW zcl_ppr_stmnt_methdef( io_scan_statement ).
        ENDCASE.
    ENDCASE.

    IF ro_statement IS NOT BOUND.
      ro_statement = NEW #( io_scan_statement ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
