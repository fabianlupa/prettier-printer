"! Processor base class
CLASS zcl_ppr_processor_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING it_source TYPE stringtab,
      process FINAL,
      get_result RETURNING VALUE(rt_formatted) TYPE stringtab.
  PROTECTED SECTION.
    METHODS:
      process_internal ABSTRACT IMPORTING it_source           TYPE stringtab
                                RETURNING VALUE(rt_formatted) TYPE stringtab.
  PRIVATE SECTION.
    DATA:
      mt_source           TYPE stringtab,
      mt_formatted_source TYPE stringtab.
ENDCLASS.



CLASS zcl_ppr_processor_base IMPLEMENTATION.
  METHOD constructor.
    mt_source = it_source.
  ENDMETHOD.

  METHOD get_result.
    rt_formatted = mt_formatted_source.
  ENDMETHOD.

  METHOD process.
    mt_formatted_source = process_internal( mt_source ).
  ENDMETHOD.
ENDCLASS.
