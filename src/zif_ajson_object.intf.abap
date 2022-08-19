interface zif_ajson_object
  public.

  methods retrieve_content
    returning
      value(ri_result) type ref to zif_ajson.

endinterface.
