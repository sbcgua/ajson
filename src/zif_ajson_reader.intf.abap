interface zif_ajson_reader
  public .

  methods exists
    importing
      iv_path type string
    returning
      value(rv_exists) type abap_bool.
  methods members
    importing
      iv_path type string
    returning
      value(rt_members) type string_table.
  methods value
    importing
      iv_path type string
    returning
      value(rv_value) type string.
  methods value_boolean
    importing
      iv_path type string
    returning
      value(rv_value) type abap_bool.
  methods value_integer
    importing
      iv_path type string
    returning
      value(rv_value) type i.
  methods value_number
    importing
      iv_path type string
    returning
      value(rv_value) type f.
  methods value_string
    importing
      iv_path type string
    returning
      value(rv_value) type string.
  methods slice
    importing
      iv_path type string
    returning
      value(ri_json) type ref to zif_ajson_reader.
  methods to_abap
    exporting
      ev_container type any
    raising
      zcx_ajson_error.

endinterface.
