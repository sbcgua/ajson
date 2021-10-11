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
        it_skip_paths type string_table optional
        iv_skip_paths type string optional
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
    field-symbols <lv_s> type string.

    if boolc( iv_skip_paths is initial ) = boolc( it_skip_paths is initial ). " XOR
      zcx_ajson_error=>raise( 'no filter path specified' ).
    endif.

    loop at it_skip_paths into lv_s.
      lv_s = to_lower( lv_s ).
      append lv_s to lt_tab.
    endloop.

    if iv_skip_paths is not initial.
      split iv_skip_paths at ',' into table lt_tab.
      loop at lt_tab assigning <lv_s>.
        if <lv_s> is initial.
          delete lt_tab index sy-tabix.
          continue.
        endif.
        <lv_s> = condense( to_lower( <lv_s> ) ).
      endloop.
    endif.

    sort lt_tab by table_line.
    delete adjacent duplicates from lt_tab.

    mt_skip_paths = lt_tab.

  endmethod.

endclass.

**********************************************************************
* MULTI FILTER
**********************************************************************

class lcl_multi_filter definition final.
  public section.
    interfaces zif_ajson_filter.
    methods constructor
      importing
        it_filters type zif_ajson_filter=>ty_filter_tab
      raising
        zcx_ajson_error.
  private section.
    data mt_filters type zif_ajson_filter=>ty_filter_tab.
endclass.

class lcl_multi_filter implementation.

  method zif_ajson_filter~keep_node.

    data li_filter like line of mt_filters.

    loop at mt_filters into li_filter.
      rv_keep = li_filter->keep_node( is_node ).
      if rv_keep = abap_false.
        return.
      endif.
    endloop.

  endmethod.

  method constructor.

    data li_filter like line of it_filters.

    loop at it_filters into li_filter where table_line is bound.
      append li_filter to mt_filters.
    endloop.

  endmethod.

endclass.
