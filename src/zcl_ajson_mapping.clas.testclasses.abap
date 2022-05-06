class ltcl_camel_case definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      to_abap for testing raising zcx_ajson_error,
      to_json for testing raising zcx_ajson_error,
      to_json_nested_struc for testing raising zcx_ajson_error,
      to_json_nested_table for testing raising zcx_ajson_error,
      to_json_first_lower for testing raising zcx_ajson_error.

endclass.


class ltcl_camel_case implementation.


  method to_abap.

    data:
      lo_ajson   type ref to zcl_ajson,
      li_mapping type ref to zif_ajson_mapping.
    data:
      begin of ls_result,
        field_data type string,
      end of ls_result.

    li_mapping = zcl_ajson_mapping=>create_camel_case( ).

    lo_ajson = zcl_ajson=>parse( iv_json = '{"FieldData":"field_value"}' ii_custom_mapping = li_mapping ).

    lo_ajson->to_abap( importing ev_container = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-field_data
      exp = 'field_value' ).

  endmethod.


  method to_json.

    data:
      lo_ajson   type ref to zcl_ajson,
      li_mapping type ref to zif_ajson_mapping.
    data:
      begin of ls_result,
        field_data type string,
      end of ls_result.

    li_mapping = zcl_ajson_mapping=>create_camel_case( iv_first_json_upper = abap_false ).

    ls_result-field_data = 'field_value'.

    lo_ajson = zcl_ajson=>create_empty( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/' iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"fieldData":"field_value"}' ).

  endmethod.


  method to_json_nested_struc.

    data:
      lo_ajson   type ref to zcl_ajson,
      li_mapping type ref to zif_ajson_mapping.
    data:
      begin of ls_result,
        field_data type string,
        begin of struc_data,
          field_more type string,
        end of struc_data,
      end of ls_result.

    li_mapping = zcl_ajson_mapping=>create_camel_case( iv_first_json_upper = abap_false ).

    ls_result-field_data = 'field_value'.
    ls_result-struc_data-field_more = 'field_more'.

    lo_ajson = zcl_ajson=>create_empty( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/' iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"fieldData":"field_value","strucData":{"fieldMore":"field_more"}}' ).

  endmethod.


  method to_json_nested_table.

    data:
      lo_ajson   type ref to zcl_ajson,
      li_mapping type ref to zif_ajson_mapping.
    data:
      lv_value type string,
      begin of ls_result,
        field_data type string,
        begin of struc_data,
          field_more type string_table,
        end of struc_data,
      end of ls_result.

    li_mapping = zcl_ajson_mapping=>create_camel_case( iv_first_json_upper = abap_false ).

    ls_result-field_data = 'field_value'.
    lv_value = 'field_more'.
    insert lv_value into table ls_result-struc_data-field_more.

    lo_ajson = zcl_ajson=>create_empty( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/'
                   iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"fieldData":"field_value","strucData":{"fieldMore":["field_more"]}}' ).

  endmethod.


  method to_json_first_lower.

    data:
      lo_ajson   type ref to zcl_ajson,
      li_mapping type ref to zif_ajson_mapping.
    data:
      begin of ls_result,
        field_data type string,
      end of ls_result.

    li_mapping = zcl_ajson_mapping=>create_camel_case( ).

    ls_result-field_data = 'field_value'.

    lo_ajson = zcl_ajson=>create_empty( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/' iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"FieldData":"field_value"}' ).

  endmethod.


endclass.



class ltcl_fields definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      to_abap for testing raising zcx_ajson_error,
      to_json for testing raising zcx_ajson_error.
endclass.


class ltcl_fields implementation.


  method to_abap.

    data:
      lo_ajson          type ref to zcl_ajson,
      li_mapping        type ref to zif_ajson_mapping,
      lt_mapping_fields type zif_ajson_mapping=>ty_mapping_fields,
      ls_mapping_field  like line of lt_mapping_fields.
    data:
      begin of ls_result,
        abap_field type string,
        field      type string,
      end of ls_result.

    clear ls_mapping_field.
    ls_mapping_field-abap  = 'ABAP_FIELD'.
    ls_mapping_field-json = 'json.field'.
    insert ls_mapping_field into table lt_mapping_fields.

    li_mapping = zcl_ajson_mapping=>create_field_mapping( lt_mapping_fields ).

    lo_ajson =
        zcl_ajson=>parse( iv_json = '{"field":"value","json.field":"field_value"}' ii_custom_mapping = li_mapping ).

    lo_ajson->to_abap( importing ev_container = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-abap_field
      exp = 'field_value' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-field
      exp = 'value' ).

  endmethod.


  method to_json.

    data:
      lo_ajson          type ref to zcl_ajson,
      li_mapping        type ref to zif_ajson_mapping,
      lt_mapping_fields type zif_ajson_mapping=>ty_mapping_fields,
      ls_mapping_field  like line of lt_mapping_fields.
    data:
      begin of ls_result,
        abap_field type string,
        field      type string,
      end of ls_result.

    clear ls_mapping_field.
    ls_mapping_field-abap  = 'ABAP_FIELD'.
    ls_mapping_field-json = 'json.field'.
    insert ls_mapping_field into table lt_mapping_fields.

    li_mapping = zcl_ajson_mapping=>create_field_mapping( lt_mapping_fields ).

    ls_result-abap_field = 'field_value'.
    ls_result-field      = 'value'.

    lo_ajson = zcl_ajson=>create_empty( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/' iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"field":"value","json.field":"field_value"}' ).

  endmethod.


endclass.



class ltcl_to_lower definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      to_json for testing raising zcx_ajson_error.
endclass.


class ltcl_to_lower implementation.


  method to_json.

    data:
      lo_ajson   type ref to zcl_ajson,
      li_mapping type ref to zif_ajson_mapping.
    data:
      begin of ls_result,
        field_data type string,
      end of ls_result.

    li_mapping = zcl_ajson_mapping=>create_lower_case( ).

    ls_result-field_data = 'field_value'.

    lo_ajson = zcl_ajson=>create_empty( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/' iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"field_data":"field_value"}' ).

  endmethod.


endclass.



class ltcl_to_upper definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      to_json for testing raising zcx_ajson_error.
endclass.


class ltcl_to_upper implementation.


  method to_json.

    data:
      lo_ajson   type ref to zcl_ajson,
      li_mapping type ref to zif_ajson_mapping.
    data:
      begin of ls_result,
        field_data type string,
      end of ls_result.

    li_mapping = zcl_ajson_mapping=>create_upper_case( ).

    ls_result-field_data = 'field_value'.

    lo_ajson = zcl_ajson=>create_empty( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/' iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"FIELD_DATA":"field_value"}' ).

  endmethod.


endclass.
