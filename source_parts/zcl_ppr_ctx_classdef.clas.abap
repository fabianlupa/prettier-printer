"! Class definition context
CLASS zcl_ppr_ctx_classdef DEFINITION
  PUBLIC
  INHERITING FROM zcl_ppr_context
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      zif_ppr_formattable~format REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_ctx_classdef IMPLEMENTATION.
  METHOD zif_ppr_formattable~format.
    rt_formatted = super->format( io_configuration ).
  ENDMETHOD.
ENDCLASS.
