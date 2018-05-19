"! Source code read error
CLASS zcx_ppr_source_read_error DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_no_arguments,
        msgid TYPE symsgid VALUE 'ZPPR',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_no_arguments,
      BEGIN OF gc_with_object,
        msgid TYPE symsgid VALUE 'ZPPR',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MV_OBJECT_TYPE',
        attr2 TYPE scx_attrname VALUE 'MV_OBJECT_NAME',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_with_object,
      BEGIN OF gc_unsupported,
        msgid TYPE symsgid VALUE 'ZPPR',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'MV_OBJECT_TYPE',
        attr2 TYPE scx_attrname VALUE 'MV_OBJECT_NAME',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_unsupported.
    INTERFACES:
      if_t100_message.
    METHODS:
      "! @parameter is_textid | Textid
      "! @parameter iv_object_type | Object type
      "! @parameter iv_object_name | Object name
      "! @parameter ix_previous | Previous exception
      constructor IMPORTING is_textid      LIKE if_t100_message=>t100key OPTIONAL
                            iv_object_type TYPE trobjtype OPTIONAL
                            iv_object_name TYPE sobj_name OPTIONAL
                            ix_previous    LIKE previous OPTIONAL.
    DATA:
      mv_object_type TYPE trobjtype READ-ONLY,
      mv_object_name TYPE sobj_name READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_ppr_source_read_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = ix_previous ).

    mv_object_type = iv_object_type.
    mv_object_name = iv_object_name.

    CLEAR me->textid.
    IF is_textid IS INITIAL.
      if_t100_message~t100key = gc_no_arguments.
    ELSE.
      if_t100_message~t100key = is_textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
