**********************************************************************
*  FILTER EMPTY VALUES
**********************************************************************

class lcl_empty_filter definition final.
  public section.
    interfaces zif_ajson_filter.
endclass.

class lcl_empty_filter implementation.
  method zif_ajson_filter~keep_node.

    rv_keep = boolc( is_node-value is initial ).
    " TODO deep !

  endmethod.
endclass.

**********************************************************************
*  FILTER PREDEFINED PATHS
**********************************************************************

class lcl_paths_filter definition final.
  public section.
    interfaces zif_ajson_filter.
    methods constructor
      importing
        it_skip_paths type string_table
      raising
        zcx_ajson_error.
  private section.
    data mt_skip_paths type hashed table of string with unique key table_line.
endclass.

class lcl_paths_filter implementation.

  method zif_ajson_filter~keep_node.

    data lv_path type string.

    lv_path = is_node-path && is_node-name.
    read table mt_skip_paths with key table_line = lv_path transporting no fields.
    rv_keep = boolc( sy-subrc <> 0 ).

  endmethod.

  method constructor.

    data lv_s type string.
    data lt_tab type string_table.

    loop at it_skip_paths into lv_s.
      lv_s = to_lower( lv_s ).
      append lv_s to lt_tab.
    endloop.

    sort lt_tab by table_line.
    delete adjacent duplicates from lt_tab.

    mt_skip_paths = lt_tab.

  endmethod.

endclass.
