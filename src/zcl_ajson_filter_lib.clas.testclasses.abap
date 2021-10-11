class ltcl_filters_test definition final
  for testing
  risk level harmless
  duration short.
  private section.
    methods empty_filter_simple for testing raising zcx_ajson_error.
    methods empty_filter_deep for testing raising zcx_ajson_error.
    methods path_filter for testing raising zcx_ajson_error.
    methods path_filter_deep for testing raising zcx_ajson_error.
    methods multi_filter for testing raising zcx_ajson_error.
endclass.


class ltcl_filters_test implementation.

  method empty_filter_simple.

    data li_json type ref to zif_ajson.

    li_json = zcl_ajson=>create_empty( )->add_node_filter(
      zcl_ajson_filter_lib=>create_empty_filter( ) ).

    li_json->set(
      iv_path = '/a'
      iv_val  = '1' ).
    li_json->set(
      iv_path = '/b'
      iv_val  = '' ).
    li_json->set(
      iv_path = '/c'
      iv_val  = '3' ).
    li_json->set(
      iv_path = '/d'
      iv_val  = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = li_json->stringify( )
      exp = '{"a":"1","c":"3"}' ).

  endmethod.

  method empty_filter_deep.

    data li_json type ref to zif_ajson.

    li_json = zcl_ajson=>create_empty( )->add_node_filter(
      zcl_ajson_filter_lib=>create_empty_filter( ) ).

    li_json->set(
      iv_path = '/a'
      iv_val  = '1' ).
    li_json->set(
      iv_path = '/b/c'
      iv_val  = '' ).
    li_json->set(
      iv_path = '/b/d'
      iv_val  = 0 ).
    li_json->set(
      iv_path = '/d/e'
      iv_val  = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = li_json->stringify( )
      exp = '{"a":"1"}' ).

  endmethod.

  method path_filter.

    data li_json type ref to zif_ajson.
    data lt_paths type string_table.

    append '/b/c' to lt_paths.

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

  method path_filter_deep.

    data li_json type ref to zif_ajson.
    data lt_paths type string_table.

    append '/b' to lt_paths.

    li_json = zcl_ajson=>create_empty( )->add_node_filter(
      zcl_ajson_filter_lib=>create_path_filter( it_skip_paths = lt_paths ) ).

    li_json->set(
      iv_path = '/a'
      iv_val  = '1' ).
    li_json->set(
      iv_path = '/b/c'
      iv_val  = '2' ).
    li_json->set(
      iv_path = '/b/d'
      iv_val  = 'x' ).
    li_json->set(
      iv_path = '/c/d'
      iv_val  = '3' ).

    cl_abap_unit_assert=>assert_equals(
      act = li_json->stringify( )
      exp = '{"a":"1","c":{"d":"3"}}' ).

  endmethod.

  method multi_filter.

    data li_json type ref to zif_ajson.
    data lt_filters type zif_ajson_filter=>ty_filter_tab.

    append zcl_ajson_filter_lib=>create_empty_filter( ) to lt_filters.
    append zcl_ajson_filter_lib=>create_path_filter( iv_skip_paths = '/c' ) to lt_filters.

    li_json = zcl_ajson=>create_empty( )->add_node_filter(
      zcl_ajson_filter_lib=>create_multi_filter( lt_filters ) ).

    li_json->set(
      iv_path = '/a'
      iv_val  = '1' ).
    li_json->set(
      iv_path = '/b'
      iv_val  = '' ).
    li_json->set(
      iv_path = '/c'
      iv_val  = '3' ).
    li_json->set(
      iv_path = '/d'
      iv_val  = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = li_json->stringify( )
      exp = '{"a":"1"}' ).

  endmethod.

endclass.
