class ltcl_mapping definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      to_abap for testing raising zcx_ajson_error,
      to_json for testing raising zcx_ajson_error.
endclass.


class ltcl_mapping implementation.


  method to_abap.

    data:
      lo_ajson   type ref to zcl_ajson,
      li_mapping type ref to zif_ajson_field_mapping.
    data:
      begin of ls_result,
        field_data type string,
      end of ls_result.

    create object li_mapping type zcl_ajson_mapping_camel.

    lo_ajson = zcl_ajson=>parse( iv_json = '{"FieldData":"field_value"}' ii_custom_mapping = li_mapping ).

    lo_ajson->to_abap( importing ev_container = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-field_data
      exp = 'field_value' ).

  endmethod.


  method to_json.

    data:
      lo_ajson   type ref to zcl_ajson,
      li_mapping type ref to zif_ajson_field_mapping.
    data:
      begin of ls_result,
        field_data type string,
      end of ls_result.

    create object li_mapping type zcl_ajson_mapping_camel.

    ls_result-field_data = 'field_value'.

    lo_ajson = zcl_ajson=>create_empty( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/' iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"FieldData":"field_value"}' ).

  endmethod.


endclass.
