interface zif_ajson_mapping
  public.

  types:
    begin of ty_rename,
      from type string,
      to type string,
    end of ty_rename,
    tty_rename_map type standard table of ty_rename
      with unique sorted key by_name components from.

  types:
    ty_table_of type standard table of ref to zif_ajson_mapping.

  methods rename_node
    importing
      !is_node type zif_ajson_types=>ty_node
    changing
      !cv_name type zif_ajson_types=>ty_node-name.

endinterface.
