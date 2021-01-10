interface zif_ajson
  public .

  constants version type string value 'v1.0.3'.
  constants origin type string value 'https://github.com/sbcgua/ajson'.

  interfaces zif_ajson_reader.
  interfaces zif_ajson_writer.

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
      children type i,
    end of ty_node .
  types:
    ty_nodes_tt type standard table of ty_node with key path name .
  types:
    ty_nodes_ts type sorted table of ty_node
      with unique key path name
      with non-unique sorted key array_index components path index .
  types:
    begin of ty_path_name,
      path type string,
      name type string,
    end of ty_path_name.

  " DATA

  data mt_json_tree type ty_nodes_ts read-only.

  " METHODS

  methods freeze.

  " METHODS (merged from reader/writer), maybe will completely move to this IF in future !

  aliases:
    exists for zif_ajson_reader~exists,
    members for zif_ajson_reader~members,
    get for zif_ajson_reader~get,
    get_boolean for zif_ajson_reader~get_boolean,
    get_integer for zif_ajson_reader~get_integer,
    get_number for zif_ajson_reader~get_number,
    get_date for zif_ajson_reader~get_date,
    get_string for zif_ajson_reader~get_string,
    slice for zif_ajson_reader~slice,
    to_abap for zif_ajson_reader~to_abap,
    array_to_string_table for zif_ajson_reader~array_to_string_table.

  aliases:
    clear for zif_ajson_writer~clear,
    set for zif_ajson_writer~set,
    set_boolean for zif_ajson_writer~set_boolean,
    set_string for zif_ajson_writer~set_string,
    set_integer for zif_ajson_writer~set_integer,
    set_date for zif_ajson_writer~set_date,
    set_null for zif_ajson_writer~set_null,
    delete for zif_ajson_writer~delete,
    touch_array for zif_ajson_writer~touch_array,
    push for zif_ajson_writer~push,
    stringify for zif_ajson_writer~stringify.

endinterface.
