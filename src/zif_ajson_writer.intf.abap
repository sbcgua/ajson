interface zif_ajson_writer
  public .

  methods set
    importing
      iv_path type string
      iv_val type any
    raising
      zcx_ajson_error.

  methods delete
    importing
      iv_path type string.

  methods touch_array
    importing
      iv_path type string
      iv_clear type abap_bool default abap_false
    raising
      zcx_ajson_error.

  methods push
    importing
      iv_path type string
      iv_val type any
    raising
      zcx_ajson_error.

  methods stringify
    returning
      value(rv_json) type string.

  methods clear.

endinterface.
