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
      lo_ajson          type ref to zcl_ajson,
      li_mapping        type ref to zif_ajson_custom_mapping,
      lt_mapping_fields type zcl_ajson_mapping_base=>ty_mapping_fields_tt,
      ls_mapping_field  like line of lt_mapping_fields.
    data:
      begin of ls_result,
        sap_field type string,
      end of ls_result.

    clear ls_mapping_field.
    ls_mapping_field-sap  = 'SAP_FIELD'.
    ls_mapping_field-json = 'json.field'.
    insert ls_mapping_field into table lt_mapping_fields.

    create object li_mapping type zcl_ajson_mapping_base
      exporting
        it_mapping_fields = lt_mapping_fields.

    lo_ajson = zcl_ajson=>parse( iv_json = '{"json.field":"field_value"}' ii_custom_mapping = li_mapping ).

    lo_ajson->to_abap( importing ev_container = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-sap_field
      exp = 'field_value' ).

  endmethod.


  method to_json.

    data:
      lo_ajson          type ref to zcl_ajson,
      li_mapping        type ref to zif_ajson_custom_mapping,
      lt_mapping_fields type zcl_ajson_mapping_base=>ty_mapping_fields_tt,
      ls_mapping_field  like line of lt_mapping_fields.
    data:
      begin of ls_result,
        sap_field type string,
      end of ls_result.

    clear ls_mapping_field.
    ls_mapping_field-sap  = 'SAP_FIELD'.
    ls_mapping_field-json = 'json.field'.
    insert ls_mapping_field into table lt_mapping_fields.

    create object li_mapping type zcl_ajson_mapping_base
      exporting
        it_mapping_fields = lt_mapping_fields.

    ls_result-sap_field = 'field_value'.

    lo_ajson = zcl_ajson=>create_empty( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/' iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"json.field":"field_value"}' ).

  endmethod.


endclass.
