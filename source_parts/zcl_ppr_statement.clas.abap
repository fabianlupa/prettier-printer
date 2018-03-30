"! Statement
CLASS zcl_ppr_statement DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      "! @parameter rv_identifier | Identifier if available
      get_identifier RETURNING VALUE(rv_identifier) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_statement IMPLEMENTATION.
  METHOD get_identifier.

  ENDMETHOD.
ENDCLASS.
