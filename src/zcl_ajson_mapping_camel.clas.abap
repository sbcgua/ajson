class zcl_ajson_mapping_camel definition
  public
  inheriting from zcl_ajson_mapping_base
  create public .

  public section.
    interfaces if_oo_adt_classrun.

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
    field-symbols <lv_token> like line of lt_tokens.

    rv_result = super->to_json( is_prefix ).

    if rv_result <> is_prefix-name. " Mapping found
      return.
    endif.

    replace all occurrences of `__` in rv_result with `*`.

    translate rv_result to lower case.
    translate rv_result using `/_:_~_`.

    split rv_result at `_` into table lt_tokens.
    loop at lt_tokens assigning <lv_token> from 2.
      translate <lv_token>(1) to upper case.
    endloop.

    concatenate lines of lt_tokens into rv_result.
    replace all occurrences of `*` in rv_result with `_`.

  endmethod.


  method if_oo_adt_classrun~main.

    constants:
      value_to_abap type string value `TestJsonField`,
      value_to_json type string value `TEST_JSON_FIELD`.

    out->write( |{ value_to_abap } => ABAP => { to_abap( iv_path = '' iv_name = '' iv_segment = value_to_abap ) }| ).
    out->write( |{ value_to_json } => JSON => { to_json( value #( name = value_to_json ) ) }| ).

  endmethod.


endclass.
