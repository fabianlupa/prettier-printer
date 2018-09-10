"! Source code provider
INTERFACE zif_ppr_source_provider PUBLIC.
  METHODS:
    get_source IMPORTING iv_object_type   TYPE trobjtype
                         iv_object_name   TYPE sobj_name
                         iv_state         TYPE r3state
               RETURNING VALUE(rt_source) TYPE stringtab
               RAISING   zcx_ppr_source_read_error.
ENDINTERFACE.
