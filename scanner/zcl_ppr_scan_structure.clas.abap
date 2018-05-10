"! Scanned Structure
CLASS zcl_ppr_scan_structure DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING is_structure   TYPE sstruc
                            iv_id          TYPE i
                            io_scan_result TYPE REF TO zcl_ppr_scan_result.
    DATA:
      ms_structure TYPE sstruc READ-ONLY,
      mv_id        TYPE i READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mo_scan_result TYPE REF TO zcl_ppr_scan_result.
ENDCLASS.



CLASS zcl_ppr_scan_structure IMPLEMENTATION.
  METHOD constructor.
    ms_structure = is_structure.
    mv_id = iv_id.
    mo_scan_result = io_scan_result.
  ENDMETHOD.
ENDCLASS.
