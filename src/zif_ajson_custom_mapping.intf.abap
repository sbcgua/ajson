interface zif_ajson_custom_mapping
  public.

  types:
    begin of ty_mapping_field_ts,
      sap  type string,
      json type string,
    end of ty_mapping_field_ts,
    ty_mapping_fields_tt type sorted table of ty_mapping_field_ts
      with unique key sap
      with unique sorted key json components json.

  methods to_abap
    importing
      !iv_path         type string
      !iv_name         type string
      !iv_segment      type string
    returning
      value(rv_result) type string.

  methods to_json
    importing
      !is_prefix       type zif_ajson=>ty_path_name
    returning
      value(rv_result) type string.

endinterface.
