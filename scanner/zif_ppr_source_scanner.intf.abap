"! ABAP source scanner
INTERFACE zif_ppr_source_scanner PUBLIC.
  METHODS:
    scan_source IMPORTING it_source     TYPE stringtab
                EXPORTING et_tokens     TYPE stokesx_tab
                          et_statements TYPE sstmnt_tab
                          et_structures TYPE sstruc_tab.
ENDINTERFACE.
