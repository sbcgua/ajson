class lcl_filter_queue definition final.
  public section.
    methods add_node_filter
      importing
        !ii_node_filter type ref to zif_ajson_filter.
    methods keep_node
      importing
        is_node type zif_ajson=>ty_node
        io_type type ref to cl_abap_typedescr
      returning
        value(rv_keep) type abap_bool
      raising
        zcx_ajson_error.
  private section.
    data mt_node_filters type standard table of ref to zif_ajson_filter.
endclass.
