class zcl_ajson_mapping_base definition
  public
  create public .

  public section.
    interfaces zif_ajson_custom_mapping.

    aliases to_abap for zif_ajson_custom_mapping~to_abap.
    aliases to_json for zif_ajson_custom_mapping~to_json.

    types:
      begin of ty_mapping_field_ts,
        sap  type string,
        json type string,
      end of ty_mapping_field_ts,
      ty_mapping_fields_tt type sorted table of ty_mapping_field_ts
        with unique key sap
        with unique sorted key json components json.

    methods constructor
      importing
        it_mapping_fields type ty_mapping_fields_tt optional.

  protected section.

  private section.
    data mt_mapping_fields type ty_mapping_fields_tt.

endclass.



class zcl_ajson_mapping_base implementation.


  method constructor.

    data ls_mapping_field like line of mt_mapping_fields.

    loop at it_mapping_fields into ls_mapping_field.
      ls_mapping_field-sap = to_upper( ls_mapping_field-sap ).
      insert ls_mapping_field into table mt_mapping_fields.
    endloop.

  endmethod.


  method zif_ajson_custom_mapping~to_abap.

    data ls_mapping_field like line of mt_mapping_fields.

    read table mt_mapping_fields into ls_mapping_field
      with key json components json = iv_segment.
    if sy-subrc = 0.
      rv_result = ls_mapping_field-sap.
    else.
      rv_result = iv_segment.
    endif.

  endmethod.


  method zif_ajson_custom_mapping~to_json.

    data lv_field type string.
    data ls_mapping_field like line of mt_mapping_fields.

    lv_field = to_upper( is_prefix-name ).

    read table mt_mapping_fields into ls_mapping_field
      with key sap = lv_field.
    if sy-subrc = 0.
      rv_result = ls_mapping_field-json.
    else.
      rv_result = is_prefix-name.
    endif.

  endmethod.


endclass.
