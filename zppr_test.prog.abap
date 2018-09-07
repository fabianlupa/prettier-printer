REPORT z_test.

PARAMETERS: p_dummy.

CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    METHODS:
      method.
    TYPES:
      type1 TYPE i,
      type2 TYPE c LENGTH 1.
    TYPES type3 TYPE n LENGTH 2.
    CLASS-METHODS class_method.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD method.
    WRITE: |Hello|.
  ENDMETHOD.

  METHOD class_method ##NEEDED.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_main( )->method( ).
