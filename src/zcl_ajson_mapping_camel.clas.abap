class zcl_ajson_mapping_camel definition
  public
  create public .

  public section.
    interfaces zif_ajson_custom_mapping.

    methods constructor
      importing
        it_mapping_fields   type zif_ajson_custom_mapping~ty_mapping_fields optional
        iv_first_json_upper type abap_bool default abap_true.

  protected section.

  private section.
    data mv_first_json_upper type abap_bool.
    data mi_mapping_fields type ref to zif_ajson_custom_mapping.

endclass.



class zcl_ajson_mapping_camel implementation.

  method constructor.

    create object mi_mapping_fields type zcl_ajson_mapping_fields
      exporting
        it_mapping_fields = it_mapping_fields.

    mv_first_json_upper = iv_first_json_upper.

  endmethod.


  method zif_ajson_custom_mapping~to_abap.

    rv_result = mi_mapping_fields->to_abap( iv_path = iv_path iv_name = iv_name ).

    if rv_result <> iv_name. " Mapping found
      return.
    endif.

    replace all occurrences of regex `([a-z])([A-Z])` in rv_result with `$1_$2`.

  endmethod.


  method zif_ajson_custom_mapping~to_json.

    types ty_token type c length 255.
    data lt_tokens type standard table of ty_token.
    data lv_from type i.
    field-symbols <token> like line of lt_tokens.

    rv_result = mi_mapping_fields->to_json( iv_path = iv_path iv_name = iv_name ).

    if rv_result <> iv_name. " Mapping found
      return.
    endif.

    replace all occurrences of `__` in rv_result with `*`.

    translate rv_result to lower case.
    translate rv_result using `/_:_~_`.

    if mv_first_json_upper = abap_true.
      lv_from = 1.
    else.
      lv_from = 2.
    endif.

    split rv_result at `_` into table lt_tokens.
    loop at lt_tokens assigning <token> from lv_from.
      translate <token>(1) to upper case.
    endloop.

    concatenate lines of lt_tokens into rv_result.
    replace all occurrences of `*` in rv_result with `_`.

  endmethod.


endclass.
