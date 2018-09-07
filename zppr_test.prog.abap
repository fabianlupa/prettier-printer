REPORT z_test.

PARAMETERS: p_dummy.

CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    METHODS:
      method.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD method.
    WRITE: |Hello|.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_main( )->method( ).
