"! Formatting rule
INTERFACE zif_ppr_formatting_rule PUBLIC.
  TYPES:
    gty_relevancy_type TYPE c LENGTH 1,
    BEGIN OF gty_relevant_for,
      type      TYPE gty_relevancy_type,
      classname TYPE abap_classname,
    END OF gty_relevant_for,
    gty_relevant_for_tab TYPE STANDARD TABLE OF gty_relevant_for WITH DEFAULT KEY.
  CONSTANTS:
    BEGIN OF gc_relevancy_type,
      all_contexts        TYPE gty_relevancy_type VALUE 'C',
      all_statements      TYPE gty_relevancy_type VALUE 'S',
      context_classname   TYPE gty_relevancy_type VALUE 'X',
      statement_classname TYPE gty_relevancy_type VALUE 'M',
    END OF gc_relevancy_type.
  EVENTS:
    before_application EXPORTING VALUE(code) TYPE REF TO stringtab,
    after_application EXPORTING VALUE(code) TYPE REF TO stringtab.
  METHODS:
    get_settings_group_name RETURNING VALUE(rv_name) TYPE ddgroup,
    get_formatting_relevancy RETURNING VALUE(rt_relevancy) TYPE gty_relevant_for_tab,
    apply_rule IMPORTING ig_settings TYPE data
                         ir_code     TYPE REF TO stringtab.
ENDINTERFACE.
