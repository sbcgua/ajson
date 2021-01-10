interface zif_ajson_custom_mapping
  public.

  methods to_abap
    importing
      !iv_path      type string
      !iv_name      type string
      !iv_segment   type string
    returning
      value(result) type string.

  methods to_json
    importing
      !is_prefix    type zif_ajson=>ty_path_name
    returning
      value(result) type string.

endinterface.
