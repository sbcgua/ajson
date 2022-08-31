interface zif_ajson_serializable
  public.

  methods serialize
    returning
      value(ri_result) type ref to zif_ajson
    raising
      zcx_ajson_error.

endinterface.
