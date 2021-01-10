class zcl_ajson_mapping_camel definition
  public
  inheriting from zcl_ajson_mapping_base
  create public .

  public section.
    methods zif_ajson_custom_mapping~to_abap redefinition.
    methods zif_ajson_custom_mapping~to_json redefinition.

  protected section.

  private section.

endclass.



class zcl_ajson_mapping_camel implementation.


  method zif_ajson_custom_mapping~to_abap.

    rv_result = super->to_abap( iv_path = iv_path iv_name = iv_name iv_segment = iv_segment ).

    if rv_result <> iv_segment. " Mapping found
      return.
    endif.

    replace all occurrences of regex `([a-z])([A-Z])` in rv_result with `$1_$2`.

    rv_result = to_upper( rv_result ).

  endmethod.


  method zif_ajson_custom_mapping~to_json.

    data lt_tokens type standard table of char256.
    field-symbols <token> like line of lt_tokens.

    rv_result = super->to_json( is_prefix ).

    if rv_result <> is_prefix-name. " Mapping found
      return.
    endif.

    replace all occurrences of `__` in rv_result with `*`.

    translate rv_result to lower case.
    translate rv_result using `/_:_~_`.

    split rv_result at `_` into table lt_tokens.
    loop at lt_tokens assigning <token> from 2.
      translate <token>(1) to upper case.
    endloop.

    concatenate lines of lt_tokens into rv_result.
    replace all occurrences of `*` in rv_result with `_`.

  endmethod.


endclass.
