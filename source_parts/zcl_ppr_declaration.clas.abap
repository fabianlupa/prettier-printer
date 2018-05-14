"! Various declaration statements
CLASS zcl_ppr_declaration DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_ppr_statement
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      gc_declaration_types LIKE zcl_ppr_constants=>gc_declaration_types
                           VALUE zcl_ppr_constants=>gc_declaration_types.
    DATA:
      mv_identifier       TYPE string READ-ONLY,
      mv_declaration_type TYPE zcl_ppr_constants=>gty_declaration_type READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_declaration IMPLEMENTATION.
ENDCLASS.
