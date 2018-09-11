REPORT z_test.

" Comment

* Another Comment

PARAMETERS: p_dummy.

DATA gv_global.

CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    METHODS method.
    METHODS
      method2.
    METHODS: method3.
    METHODS:
      method4.
    METHODS:
      method5,
      method6.
    TYPES type1 TYPE i.
    TYPES
      type2 TYPE i.
    TYPES: type3 TYPE i.
    TYPES:
      type4 TYPE i,
      type5 TYPE i.
    CLASS-METHODS class_method.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD method.
    WRITE: |Hello|.
  ENDMETHOD.

  METHOD class_method ##NEEDED.
  ENDMETHOD.

  METHOD method2.
  ENDMETHOD.

  METHOD method3.
  ENDMETHOD.

  METHOD method4.
  ENDMETHOD.

  METHOD method5.
  ENDMETHOD.

  METHOD method6.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_doc DEFINITION.
  PUBLIC SECTION.
    "! Test method
    "! @parameter iv_string | String
    "! @parameter rv_result | Result
    METHODS
      test IMPORTING iv_string        TYPE string
           RETURNING VALUE(rv_result) TYPE abap_bool.
    METHODS:
      "! Test2 method
      "! @parameter iv_string | String2
      "! @parameter rv_result | Result2
      test2 IMPORTING iv_string        TYPE string
            RETURNING VALUE(rv_result) TYPE abap_bool.
    METHODS:
      test3, " Comment
      " Comment
      test4,
      test5 ##NO_TEXT,
**********************************************************************
      test6.
    METHODS
      test7 " Mid-statement comment
        IMPORTING " Another one
          iv_test " Another one 2
            TYPE " Another one 3
              string. " After statement
ENDCLASS.

CLASS lcl_doc IMPLEMENTATION.
  METHOD test.
  ENDMETHOD.

  METHOD test2.
  ENDMETHOD.

  METHOD test3.
  ENDMETHOD.

  METHOD test4.
  ENDMETHOD.

  METHOD test5.
  ENDMETHOD.

  METHOD test6.
  ENDMETHOD.

  METHOD test7.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_main( )->method( ).
