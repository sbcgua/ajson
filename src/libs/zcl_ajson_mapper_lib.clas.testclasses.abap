class ltcl_test_mappers definition final for testing
  duration short
  risk level harmless.

  private section.

    methods:
      to_snake for testing raising zcx_ajson_error,
      to_camel for testing raising zcx_ajson_error,
      to_camel_1st_upper for testing raising zcx_ajson_error,
      rename_by_attr for testing raising zcx_ajson_error,
      rename_by_path for testing raising zcx_ajson_error,
      rename_by_pattern for testing raising zcx_ajson_error,
      compound_mapper for testing raising zcx_ajson_error,
      test_to_upper for testing raising zcx_ajson_error,
      test_to_lower for testing raising zcx_ajson_error.

endclass.


class ltcl_test_mappers implementation.

  method test_to_upper.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>parse( '{"a":1,"b":{"c":2}}'
        )->map( zcl_ajson_mapper_lib=>create_upper_case( )
        )->stringify( )
      exp = '{"A":1,"B":{"C":2}}' ).

  endmethod.

  method test_to_lower.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>parse( '{"A":1,"B":{"C":2}}'
        )->map( zcl_ajson_mapper_lib=>create_lower_case( )
        )->stringify( )
      exp = '{"a":1,"b":{"c":2}}' ).

  endmethod.

  method rename_by_attr.

    data lt_map type zif_ajson_mapper=>tty_rename_map.
    field-symbols <i> like line of lt_map.

    append initial line to lt_map assigning <i>.
    <i>-from = 'a'.
    <i>-to   = 'x'.
    append initial line to lt_map assigning <i>.
    <i>-from = 'c'.
    <i>-to   = 'y'.
    append initial line to lt_map assigning <i>.
    <i>-from = 'd'.
    <i>-to   = 'z'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>parse( '{"a":1,"b":{"c":2},"d":{"e":3}}' )->map(
        zcl_ajson_mapper_lib=>create_rename( lt_map ) )->stringify( )
      exp = '{"b":{"y":2},"x":1,"z":{"e":3}}' ).

  endmethod.

  method rename_by_path.

    data lt_map type zif_ajson_mapper=>tty_rename_map.
    field-symbols <i> like line of lt_map.

    append initial line to lt_map assigning <i>.
    <i>-from = '/b/a'.
    <i>-to   = 'x'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>parse( '{"a":1,"b":{"a":2},"c":{"a":3}}' )->map(
        zcl_ajson_mapper_lib=>create_rename(
          it_rename_map = lt_map
          iv_rename_by  = zcl_ajson_mapper_lib=>rename_by-full_path
        ) )->stringify( )
      exp = '{"a":1,"b":{"x":2},"c":{"a":3}}' ).

  endmethod.

  method rename_by_pattern.

    data lt_map type zif_ajson_mapper=>tty_rename_map.
    field-symbols <i> like line of lt_map.

    append initial line to lt_map assigning <i>.
    <i>-from = '/*/this*'.
    <i>-to   = 'x'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>parse( '{"andthisnot":1,"b":{"thisone":2},"c":{"a":3}}' )->map(
        zcl_ajson_mapper_lib=>create_rename(
          it_rename_map = lt_map
          iv_rename_by  = zcl_ajson_mapper_lib=>rename_by-pattern
        ) )->stringify( )
      exp = '{"andthisnot":1,"b":{"x":2},"c":{"a":3}}' ).

  endmethod.

  method compound_mapper.

    data lt_map type zif_ajson_mapper=>tty_rename_map.
    field-symbols <i> like line of lt_map.

    append initial line to lt_map assigning <i>.
    <i>-from = '/b/a'.
    <i>-to   = 'x'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>parse( '{"a":1,"b":{"a":2},"c":{"a":3}}' )->map(
        zcl_ajson_mapper_lib=>create_compound_mapper(
          ii_mapper1 = zcl_ajson_mapper_lib=>create_rename(
            it_rename_map = lt_map
            iv_rename_by  = zcl_ajson_mapper_lib=>rename_by-full_path )
          ii_mapper2 = zcl_ajson_mapper_lib=>create_upper_case( ) )
        )->stringify( )
      exp = '{"A":1,"B":{"X":2},"C":{"A":3}}' ).

  endmethod.

  method to_snake.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>parse( '{"aB":1,"BbC":2,"cD":{"xY":3},"ZZ":4}' )->map(
        zcl_ajson_mapper_lib=>create_to_snake_case( )
        )->stringify( )
      exp = '{"a_b":1,"bb_c":2,"c_d":{"x_y":3},"zz":4}' ).

  endmethod.

  method to_camel.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>parse( '{"a_b":1,"bb_c":2,"c_d":{"x_y":3},"zz":4}' )->map(
        zcl_ajson_mapper_lib=>create_to_camel_case( )
        )->stringify( )
      exp = '{"aB":1,"bbC":2,"cD":{"xY":3},"zz":4}' ).

    " Forced underscore
    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>parse( '{"a__b":1}' )->map(
        zcl_ajson_mapper_lib=>create_to_camel_case( )
        )->stringify( )
      exp = '{"a_b":1}' ).

  endmethod.

  method to_camel_1st_upper.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>parse( '{"aj_bc":1,"bb_c":2,"c_d":{"xq_yq":3},"zz":4}' )->map(
        zcl_ajson_mapper_lib=>create_to_camel_case( iv_first_json_upper = abap_true )
        )->stringify( )
      exp = '{"AjBc":1,"BbC":2,"CD":{"XqYq":3},"Zz":4}' ).

  endmethod.

endclass.
