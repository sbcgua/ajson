interface zif_ajson_writer
  public .

  methods set
    importing
      iv_path type string
      iv_val type any.
  " TODO
  " - ajson instance
  " - abap structure/table
  " - abap value/scalar
  " - set_bool ?

  methods touch_array
    importing
      iv_path type string.

  methods push
    importing
      iv_path type string
      iv_val type any.

  methods stringify
    returning
      value(rv_json) type string.

endinterface.
