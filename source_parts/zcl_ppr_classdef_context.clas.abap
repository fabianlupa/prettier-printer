"! Class definition context
CLASS zcl_ppr_classdef_context DEFINITION
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



CLASS zcl_ppr_classdef_context IMPLEMENTATION.
  METHOD zif_ppr_formattable~format.
    " Reorder sections
  ENDMETHOD.
ENDCLASS.
