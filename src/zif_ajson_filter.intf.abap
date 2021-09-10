interface zif_ajson_filter
  public.

  methods keep_node
    importing
      is_node type zif_ajson=>ty_node
      io_type type ref to cl_abap_typedescr
    returning
      value(rv_keep) type abap_bool
    raising
      zcx_ajson_error.

endinterface.
