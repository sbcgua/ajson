interface zif_ajson_reader
  public .

  type-pools abap.

  types:
    begin of ty_field_mapping_ts,
      sap  type string,
      json type string,
    end of ty_field_mapping_ts,
    ty_field_mapping_tt type sorted table of ty_field_mapping_ts
      with unique key sap
      with unique sorted key json components json.

  methods exists
    importing
      iv_path          type string
    returning
      value(rv_exists) type abap_bool.
  methods members
    importing
      iv_path           type string
    returning
      value(rt_members) type string_table.
  methods get
    importing
      iv_path         type string
    returning
      value(rv_value) type string.
  methods get_node_type
    importing
      iv_path             type string
    returning
      value(rv_node_type) type string.
  methods get_boolean
    importing
      iv_path         type string
    returning
      value(rv_value) type abap_bool.
  methods get_integer
    importing
      iv_path         type string
    returning
      value(rv_value) type i.
  methods get_number
    importing
      iv_path         type string
    returning
      value(rv_value) type f.
  methods get_date
    importing
      iv_path         type string
    returning
      value(rv_value) type d.
  methods get_string
    importing
      iv_path         type string
    returning
      value(rv_value) type string.
  methods slice
    importing
      iv_path        type string
    returning
      value(ri_json) type ref to zif_ajson_reader.
  methods to_abap
    importing
      it_mapping_fields type ty_field_mapping_tt optional
    exporting
      ev_container      type any
    raising
      zcx_ajson_error.
  methods array_to_string_table
    importing
      iv_path                type string
    returning
      value(rt_string_table) type string_table
    raising
      zcx_ajson_error.

endinterface.
