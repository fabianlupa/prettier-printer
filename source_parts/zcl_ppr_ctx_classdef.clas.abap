"! Class definition context
CLASS zcl_ppr_ctx_classdef DEFINITION
  PUBLIC
  INHERITING FROM zcl_ppr_context
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      gty_section_type TYPE c LENGTH 1.
    CONSTANTS:
      BEGIN OF gc_section_types,
        public    TYPE gty_section_type VALUE 'P',
        protected TYPE gty_section_type VALUE 'C',
        private   TYPE gty_section_type VALUE 'R',
*        package   TYPE gty_section_type VALUE 'D', " Please come back package visibility
      END OF gc_section_types.
    METHODS:
      get_section_by_type IMPORTING iv_type           TYPE gty_section_type
                          RETURNING VALUE(ro_section) TYPE REF TO zcl_ppr_context.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_ctx_classdef IMPLEMENTATION.
  METHOD get_section_by_type.
    LOOP AT mt_children INTO DATA(lo_child).
      IF to_upper( lo_child->get_start_statement( )->get_statement_text( ) ) = SWITCH #( iv_type
           WHEN gc_section_types-public    THEN 'PUBLIC SECTION'
           WHEN gc_section_types-protected THEN 'PROTECTED SECTION'
           WHEN gc_section_types-private   THEN 'PRIVATE SECTION'
         ).
        ro_section = lo_child.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
