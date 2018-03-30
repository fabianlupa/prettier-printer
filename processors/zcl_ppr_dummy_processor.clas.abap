"! Dummy processor
CLASS zcl_ppr_dummy_processor DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_ppr_processor_base
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS:
      process_internal REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_dummy_processor IMPLEMENTATION.
  METHOD process_internal.
    rt_formatted = it_source.
  ENDMETHOD.
ENDCLASS.
