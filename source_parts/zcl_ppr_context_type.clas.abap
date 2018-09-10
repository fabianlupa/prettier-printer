"! Context type enumeration
CLASS zcl_ppr_context_type DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,
      get_context_by_object_type IMPORTING iv_object_type         TYPE swo_objtyp
                                 RETURNING VALUE(ro_context_type) TYPE REF TO zcl_ppr_context_type.
    CLASS-DATA:
      go_unknown              TYPE REF TO zcl_ppr_context_type,
      go_class_pool           TYPE REF TO zcl_ppr_context_type,
      go_interface_pool       TYPE REF TO zcl_ppr_context_type,
      go_function_pool        TYPE REF TO zcl_ppr_context_type,
      go_type_pool            TYPE REF TO zcl_ppr_context_type,
      go_program              TYPE REF TO zcl_ppr_context_type,
      go_class_definition     TYPE REF TO zcl_ppr_context_type,
      go_class_implementation TYPE REF TO zcl_ppr_context_type,
      go_interface_definition TYPE REF TO zcl_ppr_context_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_context_type IMPLEMENTATION.
  METHOD class_constructor.

  ENDMETHOD.

  METHOD get_context_by_object_type.
    ro_context_type = SWITCH #( iv_object_type
      WHEN 'CLAS' THEN go_class_pool
      WHEN 'INTF' THEN go_interface_pool
      WHEN 'PROG' THEN go_program " ?
      ELSE go_unknown
    ).
  ENDMETHOD.
ENDCLASS.
