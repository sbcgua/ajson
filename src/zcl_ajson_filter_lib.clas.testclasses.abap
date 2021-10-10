class ltcl_filters_test definition final
  for testing
  risk level harmless
  duration short.
  private section.
    methods empty_filter_simple for testing raising zcx_ajson_error.
*    methods empty_filter_deep for testing raising zcx_ajson_error.
    methods path_filter for testing raising zcx_ajson_error.
*    methods path_filter_deep for testing raising zcx_ajson_error.
endclass.


class ltcl_filters_test implementation.

  method empty_filter_simple.

    data li_json type ref to zif_ajson.
    data lt_paths type string_table.

    append '/a/b' to lt_paths.

    li_json = zcl_ajson=>create_empty( )->add_node_filter(
      zcl_ajson_filter_lib=>create_empty_filter( ) ).

    li_json->set(
      iv_path = '/a'
      iv_val  = '1' ).
    li_json->set(
      iv_path = '/b/c'
      iv_val  = '' ).
    li_json->set(
      iv_path = '/c/d'
      iv_val  = '3' ).
    li_json->set(
      iv_path = '/d/e'
      iv_val  = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = li_json->stringify( )
      exp = '{"a":"1","c":{"d":"3"}}' ).

  endmethod.

  method path_filter.

    data li_json type ref to zif_ajson.
    data lt_paths type string_table.

    append '/a/b' to lt_paths.

    li_json = zcl_ajson=>create_empty( )->add_node_filter(
      zcl_ajson_filter_lib=>create_path_filter( it_skip_paths = lt_paths ) ).

    li_json->set(
      iv_path = '/a'
      iv_val  = '1' ).
    li_json->set(
      iv_path = '/b/c'
      iv_val  = '2' ).
    li_json->set(
      iv_path = '/c/d'
      iv_val  = '3' ).

    cl_abap_unit_assert=>assert_equals(
      act = li_json->stringify( )
      exp = '{"a":"1","c":{"d":"3"}}' ).

  endmethod.

endclass.
