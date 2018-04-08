"! Constants
CLASS zcl_ppr_constants DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CONSTANTS:
      gc_scan_stmnt_types LIKE scan_stmnt_type VALUE scan_stmnt_type,
      gc_scan_struc_types LIKE scan_struc_type VALUE scan_struc_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ppr_constants IMPLEMENTATION.
ENDCLASS.
