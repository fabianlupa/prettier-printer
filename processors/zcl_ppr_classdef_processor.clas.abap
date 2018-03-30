"! Class definition processor
CLASS zcl_ppr_classdef_processor DEFINITION
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



CLASS zcl_ppr_classdef_processor IMPLEMENTATION.
  METHOD process_internal.
    BREAK-POINT.
  ENDMETHOD.
ENDCLASS.
