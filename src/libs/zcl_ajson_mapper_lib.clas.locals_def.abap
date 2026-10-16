class lcl_rename definition.

  public section.
    interfaces zif_ajson_mapper.

    methods constructor
      importing
        it_rename_map type zif_ajson_mapper~tty_rename_map
        iv_rename_by type i.

  private section.
    data mt_rename_map type zif_ajson_mapper~tty_rename_map.
    data mv_rename_by type i.

endclass.

class lcl_mapping_to_upper definition.

  public section.
    interfaces zif_ajson_mapper.

endclass.


class lcl_mapping_to_lower definition.

  public section.
    interfaces zif_ajson_mapper.

endclass.


class lcl_compound_mapper definition.

  public section.
    interfaces zif_ajson_mapper.

    methods constructor
      importing
        it_queue type zif_ajson_mapper=>ty_table_of.

  private section.
    data mt_queue type zif_ajson_mapper=>ty_table_of.

endclass.

class lcl_to_snake definition.
  public section.
    interfaces zif_ajson_mapper.
endclass.

class lcl_to_camel definition.
  public section.
    interfaces zif_ajson_mapper.
    methods constructor
      importing
        iv_first_json_upper type abap_bool.
  private section.
    data mv_first_json_upper type abap_bool.
endclass.
