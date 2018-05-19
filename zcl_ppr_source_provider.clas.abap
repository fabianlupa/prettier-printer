"! Source code provider
CLASS zcl_ppr_source_provider DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_ppr_source_provider.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      read_report IMPORTING iv_object        TYPE sobj_name
                            iv_state         TYPE r3state
                  RETURNING VALUE(rt_source) TYPE stringtab
                  RAISING   zcx_ppr_source_read_error.
ENDCLASS.



CLASS zcl_ppr_source_provider IMPLEMENTATION.
  METHOD zif_ppr_source_provider~get_source.
    CASE iv_object_type.
      WHEN 'PROG'.
        rt_source = read_report( iv_object = iv_object_name iv_state = iv_state ).

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_ppr_source_read_error
          EXPORTING
            is_textid      = zcx_ppr_source_read_error=>gc_unsupported
            iv_object_type = iv_object_type
            iv_object_name = iv_object_name.
    ENDCASE.
  ENDMETHOD.

  METHOD read_report.
    READ REPORT iv_object INTO rt_source STATE iv_state.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ppr_source_read_error
        EXPORTING
          is_textid      = zcx_ppr_source_read_error=>gc_with_object
          iv_object_name = iv_object.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
