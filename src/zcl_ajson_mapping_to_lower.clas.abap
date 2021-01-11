class zcl_ajson_mapping_to_lower definition
  public
  create public .

  public section.
    interfaces zif_ajson_custom_mapping.

    methods constructor
      importing
        it_mapping_fields type zif_ajson_custom_mapping~ty_mapping_fields optional.

  protected section.

  private section.
    data mi_mapping_fields type ref to zif_ajson_custom_mapping.

endclass.



class zcl_ajson_mapping_to_lower implementation.

  method constructor.

    create object mi_mapping_fields type zcl_ajson_mapping_fields
      exporting
        it_mapping_fields = it_mapping_fields.

  endmethod.


  method zif_ajson_custom_mapping~to_abap.

    rv_result = mi_mapping_fields->to_abap( iv_path = iv_path iv_name = iv_name iv_segment = iv_segment ).

  endmethod.


  method zif_ajson_custom_mapping~to_json.

    data lt_tokens type standard table of char256.
    field-symbols <token> like line of lt_tokens.

    rv_result = mi_mapping_fields->to_json( is_prefix ).

    if rv_result <> is_prefix-name. " Mapping found
      return.
    endif.

    rv_result = to_lower( rv_result ).

  endmethod.


endclass.
