class zcl_ajson_mapping_fields definition
  public
  create public .

  public section.
    interfaces zif_ajson_field_mapping.

    aliases to_abap for zif_ajson_field_mapping~to_abap.
    aliases to_json for zif_ajson_field_mapping~to_json.

    methods constructor
      importing
        it_mapping_fields type zif_ajson_field_mapping~ty_mapping_fields optional.

  protected section.

  private section.
    data mt_mapping_fields type zif_ajson_field_mapping~ty_mapping_fields.

endclass.



class zcl_ajson_mapping_fields implementation.


  method constructor.

    data ls_mapping_field like line of mt_mapping_fields.

    loop at it_mapping_fields into ls_mapping_field.
      ls_mapping_field-abap = to_upper( ls_mapping_field-abap ).
      insert ls_mapping_field into table mt_mapping_fields.
    endloop.

  endmethod.


  method zif_ajson_field_mapping~to_abap.

    data ls_mapping_field like line of mt_mapping_fields.

    read table mt_mapping_fields into ls_mapping_field
      with key json components json = iv_name.
    if sy-subrc = 0.
      rv_result = ls_mapping_field-abap.
    endif.

  endmethod.


  method zif_ajson_field_mapping~to_json.

    data lv_field type string.
    data ls_mapping_field like line of mt_mapping_fields.

    lv_field = to_upper( iv_name ).

    read table mt_mapping_fields into ls_mapping_field
      with key abap = lv_field.
    if sy-subrc = 0.
      rv_result = ls_mapping_field-json.
    endif.

  endmethod.


endclass.
