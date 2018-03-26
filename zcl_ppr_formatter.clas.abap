"! Formatter
CLASS zcl_ppr_formatter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      format_source IMPORTING it_source           TYPE stringtab
                    RETURNING VALUE(rt_formatted) TYPE stringtab,
      format_object IMPORTING iv_object_type TYPE trobjtype
                              iv_object_name TYPE sobj_name.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_formatter IMPLEMENTATION.
  METHOD format_object.

  ENDMETHOD.

  METHOD format_source.

  ENDMETHOD.
ENDCLASS.
