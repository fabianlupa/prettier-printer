"! Scan result type base class
CLASS zcl_ppr_scan_return_value_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_id               TYPE i
                            iv_result_type_name TYPE string
                            io_scan_result      TYPE REF TO zcl_ppr_scan_result,
      get_description RETURNING VALUE(rv_description) TYPE string.
    DATA:
      mv_id        TYPE i READ-ONLY,
      mv_type_name TYPE string READ-ONLY.
  PROTECTED SECTION.
    DATA:
      mo_scan_result TYPE REF TO zcl_ppr_scan_result.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_scan_return_value_base IMPLEMENTATION.
  METHOD constructor.
    mv_id = iv_id.
    mv_type_name = iv_result_type_name.
    mo_scan_result = io_scan_result.
    ASSERT mo_scan_result IS BOUND.
  ENDMETHOD.

  METHOD get_description.
    rv_description = |{ mv_type_name }\{{ mv_id }\}|.
  ENDMETHOD.
ENDCLASS.
