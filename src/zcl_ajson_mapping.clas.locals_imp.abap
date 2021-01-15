class lcl_mapping_fields implementation.


  method constructor.

    data ls_mapping_field like line of mt_mapping_fields.

    loop at it_mapping_fields into ls_mapping_field.
      ls_mapping_field-abap = to_upper( ls_mapping_field-abap ).
      insert ls_mapping_field into table mt_mapping_fields.
    endloop.

  endmethod.


  method zif_ajson_mapping~to_abap.

    data ls_mapping_field like line of mt_mapping_fields.

    read table mt_mapping_fields into ls_mapping_field
      with key json components json = iv_name.
    if sy-subrc = 0.
      rv_result = ls_mapping_field-abap.
    endif.

  endmethod.


  method zif_ajson_mapping~to_json.

    data lv_field type string.
    data ls_mapping_field like line of mt_mapping_fields.

    lv_field = to_upper( iv_name ).

    read table mt_mapping_fields into ls_mapping_field
      with key abap components abap = lv_field.
    if sy-subrc = 0.
      rv_result = ls_mapping_field-json.
    endif.

  endmethod.


endclass.


class lcl_mapping_to_upper implementation.


  method constructor.

    mi_mapping_fields = zcl_ajson_mapping=>create_field_mapping( it_mapping_fields ).

  endmethod.


  method zif_ajson_mapping~to_abap.

    rv_result = mi_mapping_fields->to_abap( iv_path = iv_path iv_name = iv_name ).

  endmethod.


  method zif_ajson_mapping~to_json.

    data lt_tokens type standard table of char256.

    rv_result = mi_mapping_fields->to_json( iv_path = iv_path iv_name = iv_name ).

    if rv_result is not initial. " Mapping found
      return.
    endif.

    rv_result = to_upper( iv_name ).

  endmethod.


endclass.


class lcl_mapping_to_lower implementation.


  method constructor.

    mi_mapping_fields = zcl_ajson_mapping=>create_field_mapping( it_mapping_fields ).

  endmethod.


  method zif_ajson_mapping~to_abap.

    rv_result = mi_mapping_fields->to_abap( iv_path = iv_path iv_name = iv_name ).

  endmethod.


  method zif_ajson_mapping~to_json.

    data lt_tokens type standard table of char256.

    rv_result = mi_mapping_fields->to_json( iv_path = iv_path iv_name = iv_name ).

    if rv_result is not initial. " Mapping found
      return.
    endif.

    rv_result = to_lower( iv_name ).

  endmethod.


endclass.


class lcl_mapping_camel implementation.


  method constructor.

    mi_mapping_fields   = zcl_ajson_mapping=>create_field_mapping( it_mapping_fields ).
    mv_first_json_upper = iv_first_json_upper.

  endmethod.


  method zif_ajson_mapping~to_abap.

    rv_result = mi_mapping_fields->to_abap( iv_path = iv_path iv_name = iv_name ).

    if rv_result is not initial. " Mapping found
      return.
    endif.

    rv_result = iv_name.

    replace all occurrences of regex `([a-z])([A-Z])` in rv_result with `$1_$2`.

  endmethod.


  method zif_ajson_mapping~to_json.

    types ty_token type c length 255.
    data lt_tokens type standard table of ty_token.
    data lv_from type i.
    field-symbols <token> like line of lt_tokens.

    rv_result = mi_mapping_fields->to_json( iv_path = iv_path iv_name = iv_name ).

    if rv_result is not initial. " Mapping found
      return.
    endif.

    rv_result = iv_name.

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
