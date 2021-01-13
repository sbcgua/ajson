class lcl_mapping_fields definition.

  public section.
    interfaces zif_ajson_mapping.

    aliases to_abap for zif_ajson_mapping~to_abap.
    aliases to_json for zif_ajson_mapping~to_json.

    methods constructor
      importing
        it_mapping_fields type zif_ajson_mapping~ty_mapping_fields optional.

  protected section.

  private section.
    data mt_mapping_fields type zif_ajson_mapping~ty_mapping_fields.

endclass.


class lcl_mapping_to_upper definition.

  public section.
    interfaces zif_ajson_mapping.

    methods constructor
      importing
        it_mapping_fields type zif_ajson_mapping~ty_mapping_fields optional.

  protected section.

  private section.
    data mi_mapping_fields type ref to zif_ajson_mapping.

endclass.


class lcl_mapping_to_lower definition.

  public section.
    interfaces zif_ajson_mapping.

    methods constructor
      importing
        it_mapping_fields type zif_ajson_mapping~ty_mapping_fields optional.

  protected section.

  private section.
    data mi_mapping_fields type ref to zif_ajson_mapping.

endclass.


class lcl_mapping_camel definition.

  public section.
    interfaces zif_ajson_mapping.

    methods constructor
      importing
        it_mapping_fields   type zif_ajson_mapping~ty_mapping_fields optional
        iv_first_json_upper type abap_bool default abap_true.

  protected section.

  private section.
    data mv_first_json_upper type abap_bool.
    data mi_mapping_fields type ref to zif_ajson_mapping.

endclass.
