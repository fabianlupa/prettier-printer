"! Constants
CLASS zcl_ppr_constants DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      gty_declaration_type TYPE c LENGTH 1.
    CONSTANTS:
      gc_scan_stmnt_types LIKE scan_stmnt_type VALUE scan_stmnt_type,
      gc_scan_struc_types LIKE scan_struc_type VALUE scan_struc_type,
      gc_scan_token_types LIKE scan_token_type VALUE scan_token_type,
      BEGIN OF gc_declaration_types,
        data      TYPE gty_declaration_type VALUE 'D',
        types     TYPE gty_declaration_type VALUE 'T',
        constants TYPE gty_declaration_type VALUE 'C',
      END OF gc_declaration_types.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_constants IMPLEMENTATION.
ENDCLASS.
