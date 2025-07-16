interface zif_ajson_refs_init public.

  types:
    begin of ty_data_ref,
      path type string,
      name type string,
      dref type ref to data,
    end of ty_data_ref,
    tty_data_refs type standard table of ty_data_ref
      with unique sorted key by_path components path name.

  methods get_data_ref
    importing
      !is_node      type zif_ajson_types=>ty_node
    returning
      value(ro_ref) type ref to data.

endinterface.
