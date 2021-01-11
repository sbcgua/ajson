class ltcl_mapping definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      to_json for testing raising zcx_ajson_error.
endclass.


class ltcl_mapping implementation.


  method to_json.

    data:
      lo_ajson   type ref to zcl_ajson,
      li_mapping type ref to zif_ajson_field_mapping.
    data:
      begin of ls_result,
        field_data type string,
      end of ls_result.

    create object li_mapping type zcl_ajson_mapping_to_lower.

    ls_result-field_data = 'field_value'.

    lo_ajson = zcl_ajson=>create_empty( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/' iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"field_data":"field_value"}' ).

  endmethod.


endclass.
