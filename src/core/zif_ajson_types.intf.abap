interface zif_ajson_types
  public.

  " Types remain in a separate interface, not in zif_ajson,
  " because otherwise they create cyclic dependencies
  " e.g. ZIF_AJSON_FILTER -> ZIF_AJSON -> ZIF_AJSON_FILTER and etc.
  " So sadly not sure how to centralize the type definitions :(

  types:
    ty_node_type type string.

  constants:
    begin of node_type,
      boolean type ty_node_type value 'bool',
      string  type ty_node_type value 'str',
      number  type ty_node_type value 'num',
      null    type ty_node_type value 'null',
      array   type ty_node_type value 'array',
      object  type ty_node_type value 'object',
    end of node_type.

  types:
    begin of ty_node,
      path type string,
      name type string,
      type type ty_node_type,
      value type string,
      index type i,
      order type i,
      children type i,
    end of ty_node.
  types:
    ty_nodes_tt type standard table of ty_node with key path name.
  types:
    ty_nodes_ts type sorted table of ty_node
      with unique key path name
      with non-unique sorted key array_index components path index
      with non-unique sorted key item_order components path order.

  types:
    begin of ty_path_name,
      path type string,
      name type string,
    end of ty_path_name.

endinterface.
