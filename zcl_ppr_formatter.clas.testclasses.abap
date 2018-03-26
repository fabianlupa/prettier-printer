CLASS ltcl_class_formatter DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      simple_definition FOR TESTING,
      complex_definition FOR TESTING.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      setup,
      teardown.
    DATA:
      mo_cut TYPE REF TO zcl_ppr_formatter.
ENDCLASS.

CLASS ltcl_class_formatter IMPLEMENTATION.
  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD simple_definition.
    DATA(lt_code) = VALUE stringtab(
      ( |CLASS lcl_test DEFINITION.| )
      ( |  PUBLIC SECTION.| )
      ( |    METHODS meth1.| )
      ( |    METHODS meth2.| )
      ( |  PROTECTED SECTION.| )
      ( |  PRIVATE SECTION.| )
      ( |ENDCLASS.| )
    ).
    DATA(lt_expected_code) = VALUE stringtab(
      ( |CLASS lcl_test DEFINITION.| )
      ( |  PUBLIC SECTION.| )
      ( |    METHODS:| )
      ( |      meth1,| )
      ( |      meth2.| )
      ( |  PROTECTED SECTION.| )
      ( |  PRIVATE SECTION.| )
      ( |ENDCLASS.| )
    ).

    DATA(lt_formatted) = mo_cut->format_source( lt_code ).
    cl_abap_unit_assert=>assert_equals( exp = lt_expected_code act = lt_formatted ).
  ENDMETHOD.

  METHOD complex_definition.

  ENDMETHOD.

  METHOD teardown.
    FREE mo_cut.
  ENDMETHOD.
ENDCLASS.
