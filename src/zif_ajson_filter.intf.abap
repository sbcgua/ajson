interface zif_ajson_filter
  public.

  types ty_filter_tab type standard table of ref to zif_ajson_filter with default key.

  methods keep_node
    importing
      is_node type zif_ajson=>ty_node
    returning
      value(rv_keep) type abap_bool
    raising
      zcx_ajson_error.

endinterface.
