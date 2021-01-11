class zcl_ajson_mapping_to_lower definition
  public
  inheriting from zcl_ajson_mapping_base
  create public .

  public section.
    methods zif_ajson_custom_mapping~to_json redefinition.

  protected section.

  private section.

endclass.



class zcl_ajson_mapping_to_lower implementation.


  method zif_ajson_custom_mapping~to_json.

    data lt_tokens type standard table of char256.
    field-symbols <token> like line of lt_tokens.

    rv_result = super->to_json( is_prefix ).

    if rv_result <> is_prefix-name. " Mapping found
      return.
    endif.

    rv_result = to_lower( rv_result ).

  endmethod.


endclass.
