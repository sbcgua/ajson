interface zif_ajson
  public.

  constants version type string value 'v1.1.4'. "#EC NOTEXT
  constants origin type string value 'https://github.com/sbcgua/ajson'. "#EC NOTEXT
  constants license type string value 'MIT'. "#EC NOTEXT

  constants:
    begin of node_type,
      boolean type string value 'bool',
      string  type string value 'str',
      number  type string value 'num',
      null    type string value 'null',
      array   type string value 'array',
      object  type string value 'object',
    end of node_type.

  types:
    begin of ty_node,
      path type string,
      name type string,
      type type string,
      value type string,
      index type i,
      order type i,
      children type i,
    end of ty_node .
  types:
    ty_nodes_tt type standard table of ty_node with key path name .
  types:
    ty_nodes_ts type sorted table of ty_node
      with unique key path name
      with non-unique sorted key array_index components path index
      with non-unique sorted key item_order components path order .
  types:
    begin of ty_path_name,
      path type string,
      name type string,
    end of ty_path_name.

  " DATA

  data mt_json_tree type ty_nodes_ts read-only.

  " METHODS

  methods freeze.
  methods keep_item_order
    returning
      value(ri_json) type ref to zif_ajson.
  methods format_datetime
    importing
      iv_use_iso type abap_bool default abap_true
    returning
      value(ri_json) type ref to zif_ajson.

  " METHODS ex.reader

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

  methods get
    importing
      iv_path type string
    returning
      value(rv_value) type string.

  methods get_node_type
    importing
      iv_path type string
    returning
      value(rv_node_type) type string.

  methods get_boolean
    importing
      iv_path type string
    returning
      value(rv_value) type abap_bool.

  methods get_integer
    importing
      iv_path type string
    returning
      value(rv_value) type i.

  methods get_number
    importing
      iv_path type string
    returning
      value(rv_value) type f.

  methods get_date
    importing
      iv_path type string
    returning
      value(rv_value) type d.

  methods get_timestamp
    importing
      iv_path type string
    returning
      value(rv_value) type timestamp.

  methods get_string
    importing
      iv_path type string
    returning
      value(rv_value) type string.

  methods slice
    importing
      iv_path type string
    returning
      value(ri_json) type ref to zif_ajson.

  methods to_abap
    exporting
      ev_container type any
    raising
      zcx_ajson_error.

  methods array_to_string_table
    importing
      iv_path type string
    returning
      value(rt_string_table) type string_table
    raising
      zcx_ajson_error.

  " METHODS ex.writer

  methods clear
    raising
      zcx_ajson_error.

  methods set
    importing
      iv_path type string
      iv_val type any
      iv_ignore_empty type abap_bool default abap_true
      iv_node_type type string optional
    returning
      value(ri_json) type ref to zif_ajson
    raising
      zcx_ajson_error.

  methods set_boolean
    importing
      iv_path type string
      iv_val type any
    returning
      value(ri_json) type ref to zif_ajson
    raising
      zcx_ajson_error.

  methods set_string
    importing
      iv_path type string
      iv_val type clike
    returning
      value(ri_json) type ref to zif_ajson
    raising
      zcx_ajson_error.

  methods set_integer
    importing
      iv_path type string
      iv_val type i
    returning
      value(ri_json) type ref to zif_ajson
    raising
      zcx_ajson_error.

  methods set_date
    importing
      iv_path type string
      iv_val type d
    returning
      value(ri_json) type ref to zif_ajson
    raising
      zcx_ajson_error.

  methods set_timestamp
    importing
      iv_path type string
      iv_val type timestamp
    returning
      value(ri_json) type ref to zif_ajson
    raising
      zcx_ajson_error.

  methods set_null
    importing
      iv_path type string
    returning
      value(ri_json) type ref to zif_ajson
    raising
      zcx_ajson_error.

  methods delete
    importing
      iv_path type string
    returning
      value(ri_json) type ref to zif_ajson
    raising
      zcx_ajson_error.

  methods touch_array
    importing
      iv_path type string
      iv_clear type abap_bool default abap_false
    returning
      value(ri_json) type ref to zif_ajson
    raising
      zcx_ajson_error.

  methods push
    importing
      iv_path type string
      iv_val type any
    returning
      value(ri_json) type ref to zif_ajson
    raising
      zcx_ajson_error.

  methods stringify
    importing
      iv_indent type i default 0
    returning
      value(rv_json) type string
    raising
      zcx_ajson_error.

endinterface.
