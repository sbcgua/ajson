interface zif_ajson_iterator
  public.

  methods has_next
    returning
      value(rv_yes) type abap_bool.

  methods next
    returning
      value(ri_item) type ref to zif_ajson.

endinterface.
