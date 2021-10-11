interface zif_ajson_filter
  public.

  types ty_filter_tab type standard table of ref to zif_ajson_filter with default key.

  constants:
    begin of c_visit,
      value type i value 0,
      open  type i value 1,
      close type i value 2,
    end of c_visit.

  methods keep_node
    importing
      is_node type zif_ajson=>ty_node
      iv_visit type i default c_visit-value
    returning
      value(rv_keep) type abap_bool
    raising
      zcx_ajson_error.

endinterface.
