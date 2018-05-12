"! Scanned Structure
CLASS zcl_ppr_scan_structure DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_ppr_scan_return_value_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING is_structure   TYPE sstruc
                            iv_id          TYPE i
                            io_scan_result TYPE REF TO zcl_ppr_scan_result.
    DATA:
      ms_structure TYPE sstruc READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      gc_result_type_name TYPE string VALUE 'Structure'.
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
ENDCLASS.
