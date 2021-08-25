**********************************************************************
* UTIL
**********************************************************************
class lcl_nodes_helper definition final.
  public section.

    data mt_nodes type zif_ajson=>ty_nodes_tt.
    methods add
      importing
        iv_str type string.
    methods clear.
    methods sorted
      returning
        value(rt_nodes) type zif_ajson=>ty_nodes_ts.

endclass.

class lcl_nodes_helper implementation.
  method add.

    field-symbols <n> like line of mt_nodes.
    data lv_children type string.
    data lv_index type string.
    data lv_order type string.

    append initial line to mt_nodes assigning <n>.

    split iv_str at '|' into
      <n>-path
      <n>-name
      <n>-type
      <n>-value
      lv_index
      lv_children
      lv_order.
    condense <n>-path.
    condense <n>-name.
    condense <n>-type.
    condense <n>-value.
    <n>-index = lv_index.
    <n>-children = lv_children.
    <n>-order = lv_order.

  endmethod.

  method sorted.
    rt_nodes = mt_nodes.
  endmethod.

  method clear.
    clear mt_nodes.
  endmethod.
endclass.

**********************************************************************
* PARSER
**********************************************************************

class ltcl_parser_test definition final
  for testing
  risk level harmless
  duration short.

  public section.

    class-methods sample_json
      importing
        iv_separator type string optional
      returning
        value(rv_json) type string.

  private section.
    data mo_cut type ref to lcl_json_parser.
    data mo_nodes type ref to lcl_nodes_helper.

    methods setup.
    methods parse for testing raising zcx_ajson_error.
    methods parse_string for testing raising zcx_ajson_error.
    methods parse_number for testing raising zcx_ajson_error.
    methods parse_float for testing raising zcx_ajson_error.
    methods parse_boolean for testing raising zcx_ajson_error.
    methods parse_false for testing raising zcx_ajson_error.
    methods parse_null for testing raising zcx_ajson_error.
    methods parse_date for testing raising zcx_ajson_error.
    methods parse_bare_values for testing raising zcx_ajson_error.
    methods parse_error for testing raising zcx_ajson_error.

endclass.

class ltcl_parser_test implementation.

  method setup.
    create object mo_cut.
    create object mo_nodes.
  endmethod.

  method parse_bare_values.

    data lt_act type zif_ajson=>ty_nodes_tt.

    mo_nodes->add( ' | |str |abc | |0' ).
    lt_act = mo_cut->parse( '"abc"' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).

    mo_nodes->clear( ).
    mo_nodes->add( ' | |num |-123 | |0' ).
    lt_act = mo_cut->parse( '-123' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).

    mo_nodes->clear( ).
    mo_nodes->add( ' | |bool |true | |0' ).
    lt_act = mo_cut->parse( 'true' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).

    mo_nodes->clear( ).
    mo_nodes->add( ' | |bool |false | |0' ).
    lt_act = mo_cut->parse( 'false' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).

    mo_nodes->clear( ).
    mo_nodes->add( ' | |null | | |0' ).
    lt_act = mo_cut->parse( 'null' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).

  endmethod.

  method parse_error.

    data lt_act type zif_ajson=>ty_nodes_tt.
    data lx_err type ref to zcx_ajson_error.
    try.
      lt_act = mo_cut->parse( 'abc' ).
      cl_abap_unit_assert=>fail( 'Parsing of string w/o quotes must fail (spec)' ).
    catch zcx_ajson_error into lx_err.
      cl_abap_unit_assert=>assert_char_cp(
        act = lx_err->get_text( )
        exp = '*parsing error*' ).
      cl_abap_unit_assert=>assert_char_cp(
        act = lx_err->location
        exp = '@PARSER' ).
    endtry.

  endmethod.

  method parse_string.
    mo_nodes->add( '                 |         |object |                        |  |1' ).
    mo_nodes->add( '/                |string   |str    |abc                     |  |0' ).

    data lt_act type zif_ajson=>ty_nodes_tt.
    lt_act = mo_cut->parse( '{"string": "abc"}' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).
  endmethod.

  method parse_number.
    mo_nodes->add( '                 |         |object |                        |  |1' ).
    mo_nodes->add( '/                |number   |num    |123                     |  |0' ).

    data lt_act type zif_ajson=>ty_nodes_tt.
    lt_act = mo_cut->parse( '{"number": 123}' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).
  endmethod.

  method parse_float.
    mo_nodes->add( '                 |         |object |                        |  |1' ).
    mo_nodes->add( '/                |float    |num    |123.45                  |  |0' ).

    data lt_act type zif_ajson=>ty_nodes_tt.
    create object mo_cut.
    lt_act = mo_cut->parse( '{"float": 123.45}' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).
  endmethod.

  method parse_boolean.
    mo_nodes->add( '                 |         |object |                        |  |1' ).
    mo_nodes->add( '/                |boolean  |bool   |true                    |  |0' ).

    data lt_act type zif_ajson=>ty_nodes_tt.
    lt_act = mo_cut->parse( '{"boolean": true}' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).
  endmethod.

  method parse_false.
    mo_nodes->add( '                 |         |object |                        |  |1' ).
    mo_nodes->add( '/                |false    |bool   |false                   |  |0' ).

    data lt_act type zif_ajson=>ty_nodes_tt.
    lt_act = mo_cut->parse( '{"false": false}' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).
  endmethod.

  method parse_null.
    mo_nodes->add( '                 |         |object |                        |  |1' ).
    mo_nodes->add( '/                |null     |null   |                        |  |0' ).

    data lt_act type zif_ajson=>ty_nodes_tt.
    lt_act = mo_cut->parse( '{"null": null}' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).
  endmethod.

  method parse_date.
    mo_nodes->add( '                 |         |object |                        |  |1' ).
    mo_nodes->add( '/                |date     |str    |2020-03-15              |  |0' ).

    data lt_act type zif_ajson=>ty_nodes_tt.
    lt_act = mo_cut->parse( '{"date": "2020-03-15"}' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).
  endmethod.

  method sample_json.

    rv_json =
      '{\n' &&
      '  "string": "abc",\n' &&
      '  "number": 123,\n' &&
      '  "float": 123.45,\n' &&
      '  "boolean": true,\n' &&
      '  "false": false,\n' &&
      '  "null": null,\n' &&
      '  "date": "2020-03-15",\n' &&
      '  "issues": [\n' &&
      '    {\n' &&
      '      "message": "Indentation problem ...",\n' &&
      '      "key": "indentation",\n' &&
      '      "start": {\n' &&
      '        "row": 4,\n' &&
      '        "col": 3\n' &&
      '      },\n' &&
      '      "end": {\n' &&
      '        "row": 4,\n' &&
      '        "col": 26\n' &&
      '      },\n' &&
      '      "filename": "./zxxx.prog.abap"\n' &&
      '    },\n' &&
      '    {\n' &&
      '      "message": "Remove space before XXX",\n' &&
      '      "key": "space_before_dot",\n' &&
      '      "start": {\n' &&
      '        "row": 3,\n' &&
      '        "col": 21\n' &&
      '      },\n' &&
      '      "end": {\n' &&
      '        "row": 3,\n' &&
      '        "col": 22\n' &&
      '      },\n' &&
      '      "filename": "./zxxx.prog.abap"\n' &&
      '    }\n' &&
      '  ]\n' &&
      '}'.

    replace all occurrences of '\n' in rv_json with iv_separator.

  endmethod.

  method parse.

    data lo_cut type ref to lcl_json_parser.
    data lt_act type zif_ajson=>ty_nodes_tt.
    data lo_nodes type ref to lcl_nodes_helper.

    create object lo_nodes.
    lo_nodes->add( '                 |         |object |                        |  |8' ).
    lo_nodes->add( '/                |string   |str    |abc                     |  |0' ).
    lo_nodes->add( '/                |number   |num    |123                     |  |0' ).
    lo_nodes->add( '/                |float    |num    |123.45                  |  |0' ).
    lo_nodes->add( '/                |boolean  |bool   |true                    |  |0' ).
    lo_nodes->add( '/                |false    |bool   |false                   |  |0' ).
    lo_nodes->add( '/                |null     |null   |                        |  |0' ).
    lo_nodes->add( '/                |date     |str    |2020-03-15              |  |0' ).
    lo_nodes->add( '/                |issues   |array  |                        |  |2' ).
    lo_nodes->add( '/issues/         |1        |object |                        |1 |5' ).
    lo_nodes->add( '/issues/1/       |message  |str    |Indentation problem ... |  |0' ).
    lo_nodes->add( '/issues/1/       |key      |str    |indentation             |  |0' ).
    lo_nodes->add( '/issues/1/       |start    |object |                        |  |2' ).
    lo_nodes->add( '/issues/1/start/ |row      |num    |4                       |  |0' ).
    lo_nodes->add( '/issues/1/start/ |col      |num    |3                       |  |0' ).
    lo_nodes->add( '/issues/1/       |end      |object |                        |  |2' ).
    lo_nodes->add( '/issues/1/end/   |row      |num    |4                       |  |0' ).
    lo_nodes->add( '/issues/1/end/   |col      |num    |26                      |  |0' ).
    lo_nodes->add( '/issues/1/       |filename |str    |./zxxx.prog.abap        |  |0' ).
    lo_nodes->add( '/issues/         |2        |object |                        |2 |5' ).
    lo_nodes->add( '/issues/2/       |message  |str    |Remove space before XXX |  |0' ).
    lo_nodes->add( '/issues/2/       |key      |str    |space_before_dot        |  |0' ).
    lo_nodes->add( '/issues/2/       |start    |object |                        |  |2' ).
    lo_nodes->add( '/issues/2/start/ |row      |num    |3                       |  |0' ).
    lo_nodes->add( '/issues/2/start/ |col      |num    |21                      |  |0' ).
    lo_nodes->add( '/issues/2/       |end      |object |                        |  |2' ).
    lo_nodes->add( '/issues/2/end/   |row      |num    |3                       |  |0' ).
    lo_nodes->add( '/issues/2/end/   |col      |num    |22                      |  |0' ).
    lo_nodes->add( '/issues/2/       |filename |str    |./zxxx.prog.abap        |  |0' ).

    create object lo_cut.
    lt_act = lo_cut->parse( sample_json( ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lo_nodes->mt_nodes ).

    lt_act = lo_cut->parse( sample_json( |{ cl_abap_char_utilities=>newline }| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lo_nodes->mt_nodes ).

    lt_act = lo_cut->parse( sample_json( |{ cl_abap_char_utilities=>cr_lf }| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lo_nodes->mt_nodes ).

  endmethod.

endclass.

**********************************************************************
* SERIALIZER
**********************************************************************

class ltcl_serializer_test definition final
  for testing
  risk level harmless
  duration short.

  public section.

    class-methods sample_json
      returning
        value(rv_json) type string.
    class-methods sample_nodes
      returning
        value(rt_nodes) type zif_ajson=>ty_nodes_ts.

  private section.

    methods stringify_condensed for testing raising zcx_ajson_error.
    methods stringify_indented for testing raising zcx_ajson_error.
    methods array_index for testing raising zcx_ajson_error.
    methods item_order for testing raising zcx_ajson_error.
    methods simple_indented for testing raising zcx_ajson_error.
    methods empty_set for testing raising zcx_ajson_error.
    methods escape for testing raising zcx_ajson_error.
    methods empty for testing raising zcx_ajson_error.

endclass.

class ltcl_serializer_test implementation.

  method sample_json.

    rv_json =
      '{\n' &&
      '  "boolean": true,\n' &&
      '  "date": "2020-03-15",\n' &&
      '  "false": false,\n' &&
      '  "float": 123.45,\n' &&
      '  "issues": [\n' &&
      '    {\n' &&
      '      "end": {\n' &&
      '        "col": 26,\n' &&
      '        "row": 4\n' &&
      '      },\n' &&
      '      "filename": "./zxxx.prog.abap",\n' &&
      '      "key": "indentation",\n' &&
      '      "message": "Indentation problem ...",\n' &&
      '      "start": {\n' &&
      '        "col": 3,\n' &&
      '        "row": 4\n' &&
      '      }\n' &&
      '    },\n' &&
      '    {\n' &&
      '      "end": {\n' &&
      '        "col": 22,\n' &&
      '        "row": 3\n' &&
      '      },\n' &&
      '      "filename": "./zxxx.prog.abap",\n' &&
      '      "key": "space_before_dot",\n' &&
      '      "message": "Remove space before XXX",\n' &&
      '      "start": {\n' &&
      '        "col": 21,\n' &&
      '        "row": 3\n' &&
      '      }\n' &&
      '    }\n' &&
      '  ],\n' &&
      '  "null": null,\n' &&
      '  "number": 123,\n' &&
      '  "string": "abc"\n' &&
      '}'.

    rv_json = replace(
      val = rv_json
      sub = '\n'
      with = cl_abap_char_utilities=>newline
      occ = 0 ).

  endmethod.

  method sample_nodes.

    data lo_nodes type ref to lcl_nodes_helper.

    create object lo_nodes.
    lo_nodes->add( '                 |         |object |                        |  |8' ).
    lo_nodes->add( '/                |string   |str    |abc                     |  |0' ).
    lo_nodes->add( '/                |number   |num    |123                     |  |0' ).
    lo_nodes->add( '/                |float    |num    |123.45                  |  |0' ).
    lo_nodes->add( '/                |boolean  |bool   |true                    |  |0' ).
    lo_nodes->add( '/                |false    |bool   |false                   |  |0' ).
    lo_nodes->add( '/                |null     |null   |                        |  |0' ).
    lo_nodes->add( '/                |date     |str    |2020-03-15              |  |0' ).
    lo_nodes->add( '/                |issues   |array  |                        |  |2' ).
    lo_nodes->add( '/issues/         |1        |object |                        |1 |5' ).
    lo_nodes->add( '/issues/1/       |message  |str    |Indentation problem ... |  |0' ).
    lo_nodes->add( '/issues/1/       |key      |str    |indentation             |  |0' ).
    lo_nodes->add( '/issues/1/       |start    |object |                        |  |2' ).
    lo_nodes->add( '/issues/1/start/ |row      |num    |4                       |  |0' ).
    lo_nodes->add( '/issues/1/start/ |col      |num    |3                       |  |0' ).
    lo_nodes->add( '/issues/1/       |end      |object |                        |  |2' ).
    lo_nodes->add( '/issues/1/end/   |row      |num    |4                       |  |0' ).
    lo_nodes->add( '/issues/1/end/   |col      |num    |26                      |  |0' ).
    lo_nodes->add( '/issues/1/       |filename |str    |./zxxx.prog.abap        |  |0' ).
    lo_nodes->add( '/issues/         |2        |object |                        |2 |5' ).
    lo_nodes->add( '/issues/2/       |message  |str    |Remove space before XXX |  |0' ).
    lo_nodes->add( '/issues/2/       |key      |str    |space_before_dot        |  |0' ).
    lo_nodes->add( '/issues/2/       |start    |object |                        |  |2' ).
    lo_nodes->add( '/issues/2/start/ |row      |num    |3                       |  |0' ).
    lo_nodes->add( '/issues/2/start/ |col      |num    |21                      |  |0' ).
    lo_nodes->add( '/issues/2/       |end      |object |                        |  |2' ).
    lo_nodes->add( '/issues/2/end/   |row      |num    |3                       |  |0' ).
    lo_nodes->add( '/issues/2/end/   |col      |num    |22                      |  |0' ).
    lo_nodes->add( '/issues/2/       |filename |str    |./zxxx.prog.abap        |  |0' ).

    rt_nodes = lo_nodes->sorted( ).

  endmethod.

  method stringify_condensed.

    data lv_act type string.
    data lv_exp type string.

    lv_act = lcl_json_serializer=>stringify( sample_nodes( ) ).
    lv_exp = sample_json( ).

    lv_exp = replace(
      val = lv_exp
      sub = cl_abap_char_utilities=>newline
      with = ''
      occ = 0 ).
    condense lv_exp.
    lv_exp = replace(
      val = lv_exp
      sub = `: `
      with = ':'
      occ = 0 ).
    lv_exp = replace(
      val = lv_exp
      sub = `{ `
      with = '{'
      occ = 0 ).
    lv_exp = replace(
      val = lv_exp
      sub = `[ `
      with = '['
      occ = 0 ).
    lv_exp = replace(
      val = lv_exp
      sub = ` }`
      with = '}'
      occ = 0 ).
    lv_exp = replace(
      val = lv_exp
      sub = ` ]`
      with = ']'
      occ = 0 ).
    lv_exp = replace(
      val = lv_exp
      sub = `, `
      with = ','
      occ = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  endmethod.

  method stringify_indented.

    data lv_act type string.
    data lv_exp type string.

    lv_act = lcl_json_serializer=>stringify(
      it_json_tree = sample_nodes( )
      iv_indent    = 2 ).
    lv_exp = sample_json( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  endmethod.

  method array_index.

    data lv_act type string.
    data lv_exp type string.
    data lo_nodes type ref to lcl_nodes_helper.

    create object lo_nodes.
    lo_nodes->add( '                |    |array  |                        |  |3' ).
    lo_nodes->add( '/               |1   |str    |abc                     |2 |0' ).
    lo_nodes->add( '/               |2   |num    |123                     |1 |0' ).
    lo_nodes->add( '/               |3   |num    |123.45                  |3 |0' ).

    lv_act = lcl_json_serializer=>stringify( lo_nodes->sorted( ) ).
    lv_exp = '[123,"abc",123.45]'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  endmethod.

  method item_order.

    data lv_act type string.
    data lv_exp type string.
    data lo_nodes type ref to lcl_nodes_helper.

    create object lo_nodes.
    lo_nodes->add( '                |       |object |                   |  |3 |0' ).
    lo_nodes->add( '/               |beta   |str    |b                  |  |0 |3' ).
    lo_nodes->add( '/               |zulu   |str    |z                  |  |0 |1' ).
    lo_nodes->add( '/               |alpha  |str    |a                  |  |0 |2' ).

    lv_act = lcl_json_serializer=>stringify( lo_nodes->sorted( ) ).
    lv_exp = '{"alpha":"a","beta":"b","zulu":"z"}'. " NAME order ! (it is also a UT)

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

    lv_act = lcl_json_serializer=>stringify(
      it_json_tree = lo_nodes->sorted( )
      iv_keep_item_order = abap_true ).
    lv_exp = '{"zulu":"z","alpha":"a","beta":"b"}'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  endmethod.

  method simple_indented.

    data lv_act type string.
    data lv_exp type string.
    data lo_nodes type ref to lcl_nodes_helper.

    create object lo_nodes.
    lo_nodes->add( '                |    |array  |                        |  |3' ).
    lo_nodes->add( '/               |1   |object |                        |2 |2' ).
    lo_nodes->add( '/1/             |a   |num    |1                       |  |0' ).
    lo_nodes->add( '/1/             |b   |num    |2                       |  |0' ).
    lo_nodes->add( '/               |2   |num    |123                     |1 |0' ).
    lo_nodes->add( '/               |3   |num    |123.45                  |3 |0' ).

    lv_act = lcl_json_serializer=>stringify(
      it_json_tree = lo_nodes->sorted( )
      iv_indent    = 2 ).
    lv_exp = '[\n' &&
    '  123,\n' &&
    '  {\n' &&
    '    "a": 1,\n' &&
    '    "b": 2\n' &&
    '  },\n' &&
    '  123.45\n' &&
    ']'.
    lv_exp = replace(
      val = lv_exp
      sub = '\n'
      with = cl_abap_char_utilities=>newline
      occ = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  endmethod.

  method empty_set.

    data lv_act type string.
    data lv_exp type string.
    data lo_nodes type ref to lcl_nodes_helper.

    create object lo_nodes.
    lo_nodes->add( '                |    |array  |                        |  |0' ).

    lv_act = lcl_json_serializer=>stringify(
      it_json_tree = lo_nodes->sorted( )
      iv_indent    = 0 ).
    lv_exp = '[]'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

    lv_act = lcl_json_serializer=>stringify(
      it_json_tree = lo_nodes->sorted( )
      iv_indent    = 2 ).
    lv_exp = '[]'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  endmethod.

  method escape.

    data lv_act type string.
    data lv_exp type string.
    data lv_val type string.
    data lo_nodes type ref to lcl_nodes_helper.

    create object lo_nodes.
    lv_val = 'a' && '"' && '\' && cl_abap_char_utilities=>horizontal_tab && cl_abap_char_utilities=>cr_lf.
    lo_nodes->add( | \| \|str \|{ lv_val }\| \|0| ).

    lv_act = lcl_json_serializer=>stringify( lo_nodes->sorted( ) ).
    lv_exp = '"a\"\\\t\r\n"'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  endmethod.

  method empty.

    data lv_act type string.
    data lv_exp type string.
    data lo_nodes type ref to lcl_nodes_helper.

    create object lo_nodes.

    lv_act = lcl_json_serializer=>stringify( lo_nodes->sorted( ) ).
    lv_exp = ''.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  endmethod.

endclass.

**********************************************************************
* UTILS
**********************************************************************

class ltcl_utils_test definition final
  for testing
  risk level harmless
  duration short.

  private section.

    methods normalize_path for testing.
    methods split_path for testing.
    methods validate_array_index for testing raising zcx_ajson_error.

endclass.

class zcl_ajson definition local friends ltcl_utils_test.

class ltcl_utils_test implementation.

  method validate_array_index.

    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>validate_array_index( iv_path = 'x' iv_index = '123' )
      exp = 123 ).

    try.
      lcl_utils=>validate_array_index( iv_path = 'x' iv_index = 'a' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error.
    endtry.

    try.
      lcl_utils=>validate_array_index( iv_path = 'x' iv_index = '0' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error.
    endtry.

  endmethod.

  method normalize_path.

    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>normalize_path( '' )
      exp = '/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>normalize_path( '/' )
      exp = '/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>normalize_path( 'abc' )
      exp = '/abc/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>normalize_path( '/abc' )
      exp = '/abc/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>normalize_path( 'abc/' )
      exp = '/abc/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>normalize_path( '/abc/' )
      exp = '/abc/' ).

  endmethod.

  method split_path.

    data ls_exp type zif_ajson=>ty_path_name.
    data lv_path type string.

    lv_path     = ''. " alias to root
    ls_exp-path = ''.
    ls_exp-name = ''.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = '/'.
    ls_exp-path = ''.
    ls_exp-name = ''.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = '/abc/'.
    ls_exp-path = '/'.
    ls_exp-name = 'abc'.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = 'abc'.
    ls_exp-path = '/'.
    ls_exp-name = 'abc'.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = '/abc'.
    ls_exp-path = '/'.
    ls_exp-name = 'abc'.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = 'abc/'.
    ls_exp-path = '/'.
    ls_exp-name = 'abc'.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = '/abc/xyz'.
    ls_exp-path = '/abc/'.
    ls_exp-name = 'xyz'.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = '/abc/xyz/'.
    ls_exp-path = '/abc/'.
    ls_exp-name = 'xyz'.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>split_path( lv_path )
      exp = ls_exp ).

  endmethod.

endclass.

**********************************************************************
* READER
**********************************************************************

class ltcl_reader_test definition final
  for testing
  risk level harmless
  duration short.

  private section.

    methods get_value for testing raising zcx_ajson_error.
    methods get_node_type for testing raising zcx_ajson_error.
    methods exists for testing raising zcx_ajson_error.
    methods value_integer for testing raising zcx_ajson_error.
    methods value_number for testing raising zcx_ajson_error.
    methods value_boolean for testing raising zcx_ajson_error.
    methods value_string for testing raising zcx_ajson_error.
    methods members for testing raising zcx_ajson_error.
    methods slice for testing raising zcx_ajson_error.
    methods array_to_string_table for testing raising zcx_ajson_error.
    methods get_date for testing raising zcx_ajson_error.
    methods get_timestamp for testing raising zcx_ajson_error.

endclass.

class zcl_ajson definition local friends ltcl_reader_test.

class ltcl_reader_test implementation.

  method slice.

    data lo_cut type ref to zcl_ajson.
    data lo_nodes type ref to lcl_nodes_helper.

    create object lo_nodes.
    lo_nodes->add( '          |         |array  |                        |  |2' ).
    lo_nodes->add( '/         |1        |object |                        |1 |5' ).
    lo_nodes->add( '/1/       |message  |str    |Indentation problem ... |  |0' ).
    lo_nodes->add( '/1/       |key      |str    |indentation             |  |0' ).
    lo_nodes->add( '/1/       |start    |object |                        |  |2' ).
    lo_nodes->add( '/1/start/ |row      |num    |4                       |  |0' ).
    lo_nodes->add( '/1/start/ |col      |num    |3                       |  |0' ).
    lo_nodes->add( '/1/       |end      |object |                        |  |2' ).
    lo_nodes->add( '/1/end/   |row      |num    |4                       |  |0' ).
    lo_nodes->add( '/1/end/   |col      |num    |26                      |  |0' ).
    lo_nodes->add( '/1/       |filename |str    |./zxxx.prog.abap        |  |0' ).
    lo_nodes->add( '/         |2        |object |                        |2 |5' ).
    lo_nodes->add( '/2/       |message  |str    |Remove space before XXX |  |0' ).
    lo_nodes->add( '/2/       |key      |str    |space_before_dot        |  |0' ).
    lo_nodes->add( '/2/       |start    |object |                        |  |2' ).
    lo_nodes->add( '/2/start/ |row      |num    |3                       |  |0' ).
    lo_nodes->add( '/2/start/ |col      |num    |21                      |  |0' ).
    lo_nodes->add( '/2/       |end      |object |                        |  |2' ).
    lo_nodes->add( '/2/end/   |row      |num    |3                       |  |0' ).
    lo_nodes->add( '/2/end/   |col      |num    |22                      |  |0' ).
    lo_nodes->add( '/2/       |filename |str    |./zxxx.prog.abap        |  |0' ).


    lo_cut = zcl_ajson=>parse( ltcl_parser_test=>sample_json( ) ).
    lo_cut ?= lo_cut->zif_ajson~slice( '/issues' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

    " **********************************************************************

    create object lo_nodes.
    lo_nodes->add( '                 |         |object |                        |  |8' ).
    lo_nodes->add( '/                |string   |str    |abc                     |  |0' ).
    lo_nodes->add( '/                |number   |num    |123                     |  |0' ).
    lo_nodes->add( '/                |float    |num    |123.45                  |  |0' ).
    lo_nodes->add( '/                |boolean  |bool   |true                    |  |0' ).
    lo_nodes->add( '/                |false    |bool   |false                   |  |0' ).
    lo_nodes->add( '/                |null     |null   |                        |  |0' ).
    lo_nodes->add( '/                |date     |str    |2020-03-15              |  |0' ).
    lo_nodes->add( '/                |issues   |array  |                        |  |2' ).
    lo_nodes->add( '/issues/         |1        |object |                        |1 |5' ).
    lo_nodes->add( '/issues/1/       |message  |str    |Indentation problem ... |  |0' ).
    lo_nodes->add( '/issues/1/       |key      |str    |indentation             |  |0' ).
    lo_nodes->add( '/issues/1/       |start    |object |                        |  |2' ).
    lo_nodes->add( '/issues/1/start/ |row      |num    |4                       |  |0' ).
    lo_nodes->add( '/issues/1/start/ |col      |num    |3                       |  |0' ).
    lo_nodes->add( '/issues/1/       |end      |object |                        |  |2' ).
    lo_nodes->add( '/issues/1/end/   |row      |num    |4                       |  |0' ).
    lo_nodes->add( '/issues/1/end/   |col      |num    |26                      |  |0' ).
    lo_nodes->add( '/issues/1/       |filename |str    |./zxxx.prog.abap        |  |0' ).
    lo_nodes->add( '/issues/         |2        |object |                        |2 |5' ).
    lo_nodes->add( '/issues/2/       |message  |str    |Remove space before XXX |  |0' ).
    lo_nodes->add( '/issues/2/       |key      |str    |space_before_dot        |  |0' ).
    lo_nodes->add( '/issues/2/       |start    |object |                        |  |2' ).
    lo_nodes->add( '/issues/2/start/ |row      |num    |3                       |  |0' ).
    lo_nodes->add( '/issues/2/start/ |col      |num    |21                      |  |0' ).
    lo_nodes->add( '/issues/2/       |end      |object |                        |  |2' ).
    lo_nodes->add( '/issues/2/end/   |row      |num    |3                       |  |0' ).
    lo_nodes->add( '/issues/2/end/   |col      |num    |22                      |  |0' ).
    lo_nodes->add( '/issues/2/       |filename |str    |./zxxx.prog.abap        |  |0' ).

    lo_cut = zcl_ajson=>parse( ltcl_parser_test=>sample_json( ) ).
    lo_cut ?= lo_cut->zif_ajson~slice( '/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

    " **********************************************************************

    create object lo_nodes.
    lo_nodes->add( '  |         |object |                        | |2' ).
    lo_nodes->add( '/ |row      |num    |3                       | |0' ).
    lo_nodes->add( '/ |col      |num    |21                      | |0' ).

    lo_cut = zcl_ajson=>parse( ltcl_parser_test=>sample_json( ) ).
    lo_cut ?= lo_cut->zif_ajson~slice( '/issues/2/start/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

  endmethod.

  method get_value.

    data lo_cut type ref to zif_ajson.
    lo_cut ?= zcl_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( '/string' )
      exp = 'abc' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( '/string/' )
      exp = 'abc' ). " Hmmm ?

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( '/boolean' )
      exp = 'true' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( '/issues/2/start/row' )
      exp = '3' ).

  endmethod.

  method get_node_type.

    data li_cut type ref to zif_ajson.
    li_cut = zcl_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get_node_type( '/' )
      exp = 'object' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get_node_type( '/string' )
      exp = 'str' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get_node_type( '/number' )
      exp = 'num' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get_node_type( '/float' )
      exp = 'num' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get_node_type( '/boolean' )
      exp = 'bool' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get_node_type( '/false' )
      exp = 'bool' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get_node_type( '/null' )
      exp = 'null' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get_node_type( '/date' )
      exp = 'str' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get_node_type( '/issues' )
      exp = 'array' ).

  endmethod.

  method get_date.

    data lo_cut type ref to zcl_ajson.
    data lo_nodes type ref to lcl_nodes_helper.
    data lv_exp type d.

    create object lo_cut.
    lv_exp = '20200728'.

    create object lo_nodes.
    lo_nodes->add( '  |         |object |                        | |1' ).
    lo_nodes->add( '/ |date1    |str    |2020-07-28              | |0' ).
    lo_cut->mt_json_tree = lo_nodes->mt_nodes.

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->zif_ajson~get_date( '/date1' )
      exp = lv_exp ).

    create object lo_nodes.
    lo_nodes->add( '  |         |object |                        | |1' ).
    lo_nodes->add( '/ |date1    |str    |2020-07-28T01:00:00Z    | |0' ).
    lo_cut->mt_json_tree = lo_nodes->mt_nodes.

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->zif_ajson~get_date( '/date1' )
      exp = lv_exp ).

    create object lo_nodes.
    lo_nodes->add( '  |         |object |                        | |1' ).
    lo_nodes->add( '/ |date1    |str    |20200728                | |0' ).
    lo_cut->mt_json_tree = lo_nodes->mt_nodes.

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->zif_ajson~get_date( '/date1' )
      exp = '' ).

  endmethod.

  method get_timestamp.

    data lo_cut type ref to zcl_ajson.
    data lo_nodes type ref to lcl_nodes_helper.
    data lv_exp type timestamp value `20200728000000`.

    create object lo_cut.

    create object lo_nodes.
    lo_nodes->add( '  |         |object |                        | |1' ).
    lo_nodes->add( '/ |timestamp|str    |2020-07-28T00:00:00Z    | |0' ).
    lo_cut->mt_json_tree = lo_nodes->mt_nodes.

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->zif_ajson~get_timestamp( '/timestamp' )
      exp = lv_exp ).

  endmethod.

  method exists.

    data lo_cut type ref to zif_ajson.
    lo_cut ?= zcl_ajson=>parse( ltcl_parser_test=>sample_json( ) ).


    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->exists( '/string' )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->exists( '/string/' )
      exp = abap_true ). " mmmm ?

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->exists( '/xxx' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->exists( '/issues/2/start/row' )
      exp = abap_true ).

  endmethod.

  method value_integer.

    data lo_cut type ref to zif_ajson.
    lo_cut ?= zcl_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_integer( '/string' )
      exp = 0 ). " Hmmmm ????

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_integer( '/number' )
      exp = 123 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_integer( '/float' )
      exp = 123 ).

  endmethod.

  method value_number.

    data lo_cut type ref to zif_ajson.
    lo_cut ?= zcl_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_number( '/string' )
      exp = 0 ). " Hmmmm ????

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_number( '/number' )
      exp = +'123.0' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_number( '/float' )
      exp = +'123.45' ).

  endmethod.

  method value_boolean.

    data lo_cut type ref to zif_ajson.
    lo_cut ?= zcl_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_boolean( '/string' )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_boolean( '/number' )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_boolean( '/xxx' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_boolean( '/boolean' )
      exp = abap_true ).

  endmethod.

  method value_string.

    data lo_cut type ref to zif_ajson.
    lo_cut ?= zcl_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_string( '/string' )
      exp = 'abc' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_string( '/number' )
      exp = '123' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_string( '/xxx' )
      exp = '' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_string( '/boolean' )
      exp = 'true' ).

  endmethod.

  method members.

    data lt_exp type string_table.
    data lo_cut type ref to zif_ajson.
    lo_cut ?= zcl_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

    clear lt_exp.
    append '1' to lt_exp.
    append '2' to lt_exp.
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->members( '/issues' )
      exp = lt_exp ).

    clear lt_exp.
    append 'col' to lt_exp.
    append 'row' to lt_exp.
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->members( '/issues/1/start/' )
      exp = lt_exp ).

  endmethod.

  method array_to_string_table.

    data lo_cut type ref to zcl_ajson.
    data lo_nodes type ref to lcl_nodes_helper.
    data lt_act type string_table.
    data lt_exp type string_table.

    create object lo_nodes.
    lo_nodes->add( '  |         |array  |                        | |6' ).
    lo_nodes->add( '/ |1        |num    |123                     |1|0' ).
    lo_nodes->add( '/ |2        |num    |234                     |2|0' ).
    lo_nodes->add( '/ |3        |str    |abc                     |3|0' ).
    lo_nodes->add( '/ |4        |bool   |true                    |4|0' ).
    lo_nodes->add( '/ |5        |bool   |false                   |5|0' ).
    lo_nodes->add( '/ |6        |null   |null                    |6|0' ).

    append '123' to lt_exp.
    append '234' to lt_exp.
    append 'abc' to lt_exp.
    append 'X' to lt_exp.
    append '' to lt_exp.
    append '' to lt_exp.

    create object lo_cut.
    lo_cut->mt_json_tree = lo_nodes->mt_nodes.

    lt_act = lo_cut->zif_ajson~array_to_string_table( '/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

    " negative
    data lx type ref to zcx_ajson_error.

    create object lo_nodes.
    lo_nodes->add( '  |         |object |                        | |1' ).
    lo_nodes->add( '/ |a        |str    |abc                     | |0' ).
    lo_cut->mt_json_tree = lo_nodes->mt_nodes.

    try.
      lo_cut->zif_ajson~array_to_string_table( '/x' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Path not found: /x' ).
    endtry.

    try.
      lo_cut->zif_ajson~array_to_string_table( '/' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Array expected at: /' ).
    endtry.

    try.
      lo_cut->zif_ajson~array_to_string_table( '/a' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Array expected at: /a' ).
    endtry.

    create object lo_nodes.
    lo_nodes->add( '  |         |array  |                        | |1' ).
    lo_nodes->add( '/ |1        |object |                        |1|0' ).
    lo_cut->mt_json_tree = lo_nodes->mt_nodes.

    try.
      lo_cut->zif_ajson~array_to_string_table( '/' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Cannot convert [object] to string at [/1]' ).
    endtry.

  endmethod.

endclass.


**********************************************************************
* JSON TO ABAP
**********************************************************************

class ltcl_json_to_abap definition
  for testing
  risk level harmless
  duration short
  final.

  private section.

    types:
      begin of ty_struc,
        a type string,
        b type i,
      end of ty_struc,
      tty_struc type standard table of ty_struc with default key,
      begin of ty_complex,
        str   type string,
        int   type i,
        float type f,
        bool  type abap_bool,
        obj   type ty_struc,
        tab   type tty_struc,
        oref  type ref to object,
        date1 type d,
        date2 type d,
        timestamp1 type timestamp,
        timestamp2 type timestamp,
        timestamp3 type timestamp,
      end of ty_complex.

    methods find_loc for testing raising zcx_ajson_error.
    methods find_loc_negative for testing.
    methods find_loc_append for testing raising zcx_ajson_error.
    methods to_abap for testing raising zcx_ajson_error.
    methods to_abap_negative for testing.

    methods prepare_cut
      exporting
        eo_cut type ref to lcl_json_to_abap
        e_elem type ty_struc
        e_mock type ty_complex.

endclass.

class ltcl_json_to_abap implementation.

  method prepare_cut.

    e_mock-str = 'Hello'.
    e_mock-int = 10.
    e_mock-obj-a = 'World'.
    e_elem-a = 'One'.
    e_elem-b = 1.
    append e_elem to e_mock-tab.
    e_elem-a = 'two'.
    e_elem-b = 2.
    append e_elem to e_mock-tab.

    lcl_json_to_abap=>bind(
      changing
        c_obj = e_mock
        co_instance = eo_cut ).

  endmethod.

  method find_loc.

    data last_elem type ty_struc.
    data ls_mock type ty_complex.
    data lo_cut type ref to lcl_json_to_abap.

    prepare_cut(
      importing
        eo_cut = lo_cut
        e_mock = ls_mock
        e_elem = last_elem ).

    data lr_ref type ref to data.
    field-symbols <val> type any.

    lr_ref = lo_cut->find_loc( 'str' ). " Relative also works but from root
    assign lr_ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 'Hello' ).

    lr_ref = lo_cut->find_loc( '/str' ).
    assign lr_ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 'Hello' ).

    lr_ref = lo_cut->find_loc( '/int' ).
    assign lr_ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 10 ).

    lr_ref = lo_cut->find_loc( '/obj/a' ).
    assign lr_ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 'World' ).

    lr_ref = lo_cut->find_loc( iv_path = '/obj' iv_name = 'a' ).
    assign lr_ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 'World' ).

    lr_ref = lo_cut->find_loc( '/obj' ).
    assign lr_ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = ls_mock-obj ).

    lr_ref = lo_cut->find_loc( '/' ).
    assign lr_ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = ls_mock ).

    lr_ref = lo_cut->find_loc( '/tab/2' ).
    assign lr_ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = last_elem ).

    lr_ref = lo_cut->find_loc( '/tab/1/a' ).
    assign lr_ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 'One' ).

  endmethod.

  method find_loc_append.

    data last_elem type ty_struc.
    data ls_mock type ty_complex.
    data lo_cut type ref to lcl_json_to_abap.
    data lx type ref to zcx_ajson_error.

    prepare_cut(
      importing
        eo_cut = lo_cut
        e_mock = ls_mock
        e_elem = last_elem ).

    data lr_ref type ref to data.
    field-symbols <val> type any.

    lr_ref = lo_cut->find_loc( '/tab/1/a' ).
    assign lr_ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 'One' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_mock-tab )
      exp = 2 ).

    try.
      lo_cut->find_loc( '/tab/3/a' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Index not found in table' ).
    endtry.

    lr_ref = lo_cut->find_loc( iv_path = '/tab/3/a' iv_append_tables = abap_true ).
    assign lr_ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = '' ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_mock-tab )
      exp = 3 ).

    try.
      lo_cut->find_loc( '/tab/5/a' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Index not found in table' ).
    endtry.

  endmethod.

  method find_loc_negative.

    data lo_cut type ref to lcl_json_to_abap.
    data lx type ref to zcx_ajson_error.
    data ls_mock type ty_complex.

    prepare_cut(
      importing
        e_mock = ls_mock " Must be here to keep reference alive
        eo_cut = lo_cut ).

    try.
      lo_cut->find_loc( '/xyz' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Path not found' ).
    endtry.

    try.
      lo_cut->find_loc( '/oref/xyz' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Cannot assign to ref' ).
    endtry.

    try.
      lo_cut->find_loc( '/tab/xyz' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Need index to access tables' ).
    endtry.

    try.
      lo_cut->find_loc( '/tab/5' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Index not found in table' ).
    endtry.

  endmethod.

  method to_abap.

    data lo_cut type ref to lcl_json_to_abap.
    data ls_mock type ty_complex.
    data lv_exp_date type d value '20200728'.
    data lv_exp_timestamp type timestamp value '20200728000000'.
    lcl_json_to_abap=>bind(
      changing
        c_obj = ls_mock
        co_instance = lo_cut ).

    data lo_nodes type ref to lcl_nodes_helper.
    create object lo_nodes.
    lo_nodes->add( '/      |           |object |                          | ' ).
    lo_nodes->add( '/      |str        |str    |hello                     | ' ).
    lo_nodes->add( '/      |int        |num    |5                         | ' ).
    lo_nodes->add( '/      |float      |num    |5.5                       | ' ).
    lo_nodes->add( '/      |bool       |bool   |true                      | ' ).
    lo_nodes->add( '/      |obj        |object |                          | ' ).
    lo_nodes->add( '/obj   |a          |str    |world                     | ' ).
    lo_nodes->add( '/      |tab        |array  |                          | ' ).
    lo_nodes->add( '/tab   |1          |object |                          |1' ).
    lo_nodes->add( '/tab/1 |a          |str    | One                      | ' ).
    lo_nodes->add( '/tab   |2          |object |                          |2' ).
    lo_nodes->add( '/tab/2 |a          |str    | Two                      | ' ).
    lo_nodes->add( '/      |date1      |str    |2020-07-28                | ' ).
    lo_nodes->add( '/      |date2      |str    |2020-07-28T00:00:00Z      | ' ).
    lo_nodes->add( '/      |timestamp1 |str    |2020-07-28T00:00:00       | ' ).
    lo_nodes->add( '/      |timestamp2 |str    |2020-07-28T00:00:00Z      | ' ).
    lo_nodes->add( '/      |timestamp3 |str    |2020-07-28T01:00:00+01:00 | ' ).

    lo_cut->to_abap( lo_nodes->sorted( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-str
      exp = 'hello' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-int
      exp = 5 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-float
      exp = '5.5' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-bool
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-obj-a
      exp = 'world' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-date1
      exp = lv_exp_date ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-date2
      exp = lv_exp_date ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-timestamp1
      exp = lv_exp_timestamp ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-timestamp2
      exp = lv_exp_timestamp ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-timestamp3
      exp = lv_exp_timestamp ).

    data ls_elem like line of ls_mock-tab.
    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_mock-tab )
      exp = 2 ).

    read table ls_mock-tab into ls_elem index 1.
    cl_abap_unit_assert=>assert_equals(
      act = ls_elem-a
      exp = 'One' ).
    read table ls_mock-tab into ls_elem index 2.
    cl_abap_unit_assert=>assert_equals(
      act = ls_elem-a
      exp = 'Two' ).

  endmethod.

  method to_abap_negative.

    data lo_cut type ref to lcl_json_to_abap.
    data lx type ref to zcx_ajson_error.
    data ls_mock type ty_complex.
    lcl_json_to_abap=>bind(
      changing
        c_obj = ls_mock
        co_instance = lo_cut ).

    data lo_nodes type ref to lcl_nodes_helper.

    try.
      create object lo_nodes.
      lo_nodes->add( '/    |      |object | ' ).
      lo_nodes->add( '/    |str   |object | ' ).

      lo_cut->to_abap( lo_nodes->sorted( ) ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Expected structure' ).
    endtry.

    try.
      create object lo_nodes.
      lo_nodes->add( '/    |      |object | ' ).
      lo_nodes->add( '/    |str   |array  | ' ).

      lo_cut->to_abap( lo_nodes->sorted( ) ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Expected table' ).
    endtry.

    try.
      create object lo_nodes.
      lo_nodes->add( '/    |      |object |      ' ).
      lo_nodes->add( '/    |int   |str    |hello ' ).

      lo_cut->to_abap( lo_nodes->sorted( ) ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Source is not a number' ).
    endtry.

    try.
      create object lo_nodes.
      lo_nodes->add( '/    |      |object |        ' ).
      lo_nodes->add( '/    |date1 |str    |baddate ' ).

      lo_cut->to_abap( lo_nodes->sorted( ) ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Unexpected date format' ).
    endtry.

  endmethod.

endclass.

**********************************************************************
* WRITER
**********************************************************************

class ltcl_writer_test definition final
  for testing
  risk level harmless
  duration short.

  private section.

    methods set_ajson for testing raising zcx_ajson_error.
    methods set_value for testing raising zcx_ajson_error.
    methods ignore_empty for testing raising zcx_ajson_error.
    methods set_obj for testing raising zcx_ajson_error.
    methods set_tab for testing raising zcx_ajson_error.
    methods set_tab_hashed for testing raising zcx_ajson_error.
    methods prove_path_exists for testing raising zcx_ajson_error.
    methods delete_subtree for testing raising zcx_ajson_error.
    methods delete for testing raising zcx_ajson_error.
    methods arrays for testing raising zcx_ajson_error.
    methods arrays_negative for testing raising zcx_ajson_error.
    methods root_assignment for testing raising zcx_ajson_error.
    methods set_bool for testing raising zcx_ajson_error.
    methods set_str for testing raising zcx_ajson_error.
    methods set_int for testing raising zcx_ajson_error.
    methods set_date for testing raising zcx_ajson_error.
    methods set_timestamp for testing raising zcx_ajson_error.
    methods read_only for testing raising zcx_ajson_error.
    methods set_array_obj for testing raising zcx_ajson_error.
    methods set_with_type for testing raising zcx_ajson_error.
    methods set_with_type_slice
      importing
        io_json_in type ref to zcl_ajson
        io_json_out type ref to zif_ajson
        iv_path type string
      raising
        zcx_ajson_error.

endclass.

class zcl_ajson definition local friends ltcl_writer_test.

class ltcl_writer_test implementation.

  method prove_path_exists.

    data lo_cut type ref to zcl_ajson.
    data lo_nodes_exp type ref to lcl_nodes_helper.

    lo_cut = zcl_ajson=>create_empty( ).

    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     ||1' ).
    lo_nodes_exp->add( '/       |a     |object |     ||1' ).
    lo_nodes_exp->add( '/a/     |b     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/   |c     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/c/ |d     |object |     ||0' ).

    lo_cut->prove_path_exists( '/a/b/c/d/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    create object lo_nodes_exp.
    lo_nodes_exp->add( '         |      |object |     ||1' ).
    lo_nodes_exp->add( '/        |a     |object |     ||1' ).
    lo_nodes_exp->add( '/a/      |b     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/    |c     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/c/  |d     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/c/d |e     |object |     ||0' ).
    lo_cut->prove_path_exists( '/a/b/c/d/e/' ).

  endmethod.

  method delete_subtree.

    data lo_cut type ref to zcl_ajson.
    data lo_nodes_exp type ref to lcl_nodes_helper.

    lo_cut = zcl_ajson=>create_empty( ).

    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     ||1' ).
    lo_nodes_exp->add( '/       |a     |object |     ||1' ).
    lo_nodes_exp->add( '/a/     |b     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/   |c     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/c/ |d     |object |     ||0' ).

    lo_cut->mt_json_tree = lo_nodes_exp->mt_nodes.

    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     ||1' ).
    lo_nodes_exp->add( '/       |a     |object |     ||0' ).

    lo_cut->delete_subtree(
      iv_path = '/a/'
      iv_name = 'b' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  endmethod.

  method delete.

    data lo_cut type ref to zcl_ajson.
    data lo_nodes_exp type ref to lcl_nodes_helper.

    lo_cut = zcl_ajson=>create_empty( ).

    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     ||1' ).
    lo_nodes_exp->add( '/       |a     |object |     ||1' ).
    lo_nodes_exp->add( '/a/     |b     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/   |c     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/c/ |d     |object |     ||0' ).

    lo_cut->mt_json_tree = lo_nodes_exp->mt_nodes.

    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     ||1' ).
    lo_nodes_exp->add( '/       |a     |object |     ||0' ).

    lo_cut->zif_ajson~delete( iv_path = '/a/b' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     ||1' ).
    lo_nodes_exp->add( '/       |a     |object |     ||1' ).
    lo_nodes_exp->add( '/a/     |b     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/   |c     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/c/ |d     |object |     ||0' ).

    lo_cut->mt_json_tree = lo_nodes_exp->mt_nodes.

    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     ||1' ).
    lo_nodes_exp->add( '/       |a     |object |     ||0' ).

    lo_cut->zif_ajson~delete( iv_path = '/a/b/' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  endmethod.

  method set_ajson.

    data lo_nodes type ref to lcl_nodes_helper.
    data lo_src type ref to zcl_ajson.
    data lo_cut type ref to zcl_ajson.
    data li_writer type ref to zif_ajson.

    lo_src = zcl_ajson=>create_empty( ).
    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.

    " Prepare source
    create object lo_nodes.
    lo_nodes->add( '        |      |object |     ||1' ).
    lo_nodes->add( '/       |x     |object |     ||2' ).
    lo_nodes->add( '/x/     |b     |str    |abc  ||0' ).
    lo_nodes->add( '/x/     |c     |num    |10   ||0' ).
    lo_src->mt_json_tree = lo_nodes->mt_nodes.

    " Test 1 - assign root
    li_writer->set(
      iv_path = ''
      iv_val  = lo_src ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

    li_writer->set(
      iv_path = '/'
      iv_val  = lo_src ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

    " Test 2 - assign deep
    create object lo_nodes.
    lo_nodes->add( '        |      |object |     ||1' ).
    lo_nodes->add( '/       |a     |object |     ||1' ).
    lo_nodes->add( '/a/     |b     |object |     ||1' ).
    lo_nodes->add( '/a/b/     |c     |object |     ||1' ).
    lo_nodes->add( '/a/b/c/   |x     |object |     ||2' ).
    lo_nodes->add( '/a/b/c/x/ |b     |str    |abc  ||0' ).
    lo_nodes->add( '/a/b/c/x/ |c     |num    |10   ||0' ).

    li_writer->clear( ).
    li_writer->set(
      iv_path = '/a/b/c'
      iv_val  = lo_src ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

    " Test 3 - assign rewrite
    create object lo_nodes.
    lo_nodes->add( '        |      |object |     ||1' ).
    lo_nodes->add( '/       |a     |object |     ||1' ).
    lo_nodes->add( '/a/       |b     |object |     ||1' ).
    lo_nodes->add( '/a/b/     |x     |object |     ||2' ).
    lo_nodes->add( '/a/b/x/   |b     |str    |abc  ||0' ).
    lo_nodes->add( '/a/b/x/   |c     |num    |10   ||0' ).

    li_writer->set(
      iv_path = '/a/b'
      iv_val  = lo_src ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

  endmethod.

  method set_value.

    data lo_nodes type ref to lcl_nodes_helper.
    data lo_cut type ref to zcl_ajson.
    data li_writer type ref to zif_ajson.

    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.

    " Prepare source
    create object lo_nodes.
    lo_nodes->add( '        |      |object |     ||1' ).
    lo_nodes->add( '/       |x     |object |     ||2' ).
    lo_nodes->add( '/x/     |b     |str    |abc  ||0' ).
    lo_nodes->add( '/x/     |c     |num    |10   ||0' ).

    li_writer->set(
      iv_path = '/x/b'
      iv_val  = 'abc' ).
    li_writer->set(
      iv_path = '/x/c'
      iv_val  = 10 ).
    li_writer->set( " ignore empty
      iv_path = '/x/d'
      iv_val  = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

  endmethod.

  method ignore_empty.

    data lo_nodes type ref to lcl_nodes_helper.
    data lo_cut type ref to zcl_ajson.
    data li_writer type ref to zif_ajson.

    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.

    create object lo_nodes.
    lo_nodes->add( '        |      |object |     ||1' ).
    lo_nodes->add( '/       |a     |num    |1    ||0' ).

    li_writer->set(
      iv_path = '/a'
      iv_val  = 1 ).
    li_writer->set( " ignore empty
      iv_path = '/b'
      iv_val  = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

    create object lo_nodes.
    lo_nodes->add( '        |      |object |     ||2' ).
    lo_nodes->add( '/       |a     |num    |1    ||0' ).
    lo_nodes->add( '/       |b     |num    |0    ||0' ).

    li_writer->set(
      iv_ignore_empty = abap_false
      iv_path = '/b'
      iv_val  = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

  endmethod.

  method set_obj.

    data lo_nodes type ref to lcl_nodes_helper.
    data lo_cut type ref to zcl_ajson.
    data li_writer type ref to zif_ajson.

    data:
      begin of ls_struc,
        b type string value 'abc',
        c type i value 10,
      end of ls_struc.

    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.

    " Prepare source
    create object lo_nodes.
    lo_nodes->add( '        |      |object |     ||1' ).
    lo_nodes->add( '/       |x     |object |     ||2' ).
    lo_nodes->add( '/x/     |b     |str    |abc  ||0' ).
    lo_nodes->add( '/x/     |c     |num    |10   ||0' ).

    li_writer->set(
      iv_path = '/x'
      iv_val  = ls_struc ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

  endmethod.

  method set_tab.

    data lo_nodes type ref to lcl_nodes_helper.
    data lo_cut type ref to zcl_ajson.
    data li_writer type ref to zif_ajson.
    data lt_tab type string_table.

    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.

    append 'hello' to lt_tab.
    append 'world' to lt_tab.

    " Prepare source
    create object lo_nodes.
    lo_nodes->add( '        |      |object |     | |1' ).
    lo_nodes->add( '/       |x     |array  |     | |2' ).
    lo_nodes->add( '/x/     |1     |str    |hello|1|0' ).
    lo_nodes->add( '/x/     |2     |str    |world|2|0' ).

    li_writer->set(
      iv_path = '/x'
      iv_val  = lt_tab ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

  endmethod.

  method set_tab_hashed.

    data lo_nodes type ref to lcl_nodes_helper.
    data lo_cut type ref to zcl_ajson.
    data li_writer type ref to zif_ajson.
    data lt_tab type hashed table of string with unique default key.

    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.

    insert `hello` into table lt_tab.
    insert `world` into table lt_tab.

    " Prepare source
    create object lo_nodes.
    lo_nodes->add( '        |      |object |     | |1' ).
    lo_nodes->add( '/       |x     |array  |     | |2' ).
    lo_nodes->add( '/x/     |1     |str    |hello|1|0' ).
    lo_nodes->add( '/x/     |2     |str    |world|2|0' ).

    li_writer->set(
      iv_path = '/x'
      iv_val  = lt_tab ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

  endmethod.

  method arrays.

    data lo_cut type ref to zcl_ajson.
    data lo_nodes_exp type ref to lcl_nodes_helper.
    data li_writer type ref to zif_ajson.

    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.

    " touch
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     | |1' ).
    lo_nodes_exp->add( '/       |a     |array  |     | |0' ).

    li_writer->touch_array( iv_path = '/a' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " add string
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     | |1' ).
    lo_nodes_exp->add( '/       |a     |array  |     | |1' ).
    lo_nodes_exp->add( '/a/     |1     |str    |hello|1|0' ).

    li_writer->push(
      iv_path = '/a'
      iv_val  = 'hello' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " add obj
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     | |1' ).
    lo_nodes_exp->add( '/       |a     |array  |     | |2' ).
    lo_nodes_exp->add( '/a/     |1     |str    |hello|1|0' ).
    lo_nodes_exp->add( '/a/     |2     |object |     |2|1' ).
    lo_nodes_exp->add( '/a/2/   |x     |str    |world| |0' ).

    data:
      begin of ls_dummy,
        x type string value 'world',
      end of ls_dummy.

    li_writer->push(
      iv_path = '/a'
      iv_val  = ls_dummy ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " re-touch
    li_writer->touch_array( iv_path = '/a' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " re-touch with clear
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     | |1' ).
    lo_nodes_exp->add( '/       |a     |array  |     | |0' ).

    li_writer->touch_array(
      iv_path = '/a'
      iv_clear = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " free-add array item (index must be updated)
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     | |1' ).
    lo_nodes_exp->add( '/       |a     |array  |     | |2' ).
    lo_nodes_exp->add( '/a/     |1     |object |     |1|1' ).
    lo_nodes_exp->add( '/a/1/   |x     |num    |123  | |0' ).
    lo_nodes_exp->add( '/a/     |2     |num    |234  |2|0' ).

    li_writer->set(
      iv_path = '/a/1/x'
      iv_val  = 123 ).
    li_writer->set(
      iv_path = '/a/2'
      iv_val  = 234 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  endmethod.

  method arrays_negative.

    data lo_cut type ref to zcl_ajson.
    data li_writer type ref to zif_ajson.

    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.

    li_writer->touch_array( iv_path = '/a' ).
    li_writer->push(
      iv_path = '/a'
      iv_val = 123 ).

    " touch another node
    data lx type ref to zcx_ajson_error.
    try.
      li_writer->touch_array( iv_path = '/a/1' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Path [/a/1] already used and is not array' ).
    endtry.

    " push to not array
    try.
      li_writer->push(
        iv_path = '/a/1'
        iv_val  = 123 ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Path [/a/1] is not array' ).
    endtry.

    " push to not array
    try.
      li_writer->push(
        iv_path = '/x'
        iv_val  = 123 ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Path [/x] does not exist' ).
    endtry.

    " set array item with non-numeric key
    try.
      li_writer->set(
        iv_path = '/a/abc/x'
        iv_val  = 123 ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Cannot add non-numeric key [abc] to array [/a/]' ).
    endtry.

    try.
      li_writer->set(
        iv_path = '/a/abc'
        iv_val  = 123 ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Cannot add non-numeric key [abc] to array [/a/]' ).
    endtry.

    " set array item with zero key
    try.
      li_writer->set(
        iv_path = '/a/0'
        iv_val  = 123 ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Cannot add zero key to array [/a/]' ).
    endtry.

  endmethod.


  method root_assignment.

    data lo_cut type ref to zcl_ajson.
    data lo_nodes_exp type ref to lcl_nodes_helper.
    data li_writer type ref to zif_ajson.
    data:
      begin of ls_dummy,
        x type string value 'hello',
      end of ls_dummy.

    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.

    " object
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     ||1' ).
    lo_nodes_exp->add( '/       |x     |str    |hello||0' ).

    li_writer->set(
      iv_path = '/'
      iv_val  = ls_dummy ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " object empty path
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     ||1' ).
    lo_nodes_exp->add( '/       |x     |str    |hello||0' ).

    li_writer->clear( ).
    li_writer->set(
      iv_path = ''
      iv_val  = ls_dummy ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " array
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |array  |     | |1' ).
    lo_nodes_exp->add( '/       |1     |str    |hello|1|0' ).

    li_writer->clear( ).
    li_writer->touch_array( iv_path = '' ).
    li_writer->push(
      iv_path = ''
      iv_val  = 'hello' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " value
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |str    |hello||0' ).

    li_writer->clear( ).
    li_writer->set(
      iv_path = ''
      iv_val  = 'hello' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  endmethod.

  method set_bool.

    data lo_cut type ref to zcl_ajson.
    data lo_nodes_exp type ref to lcl_nodes_helper.
    data li_writer type ref to zif_ajson.
    data lt_tab type string_table.

    " abap_bool
    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |      ||2' ).
    lo_nodes_exp->add( '/       |a     |bool   |true  ||0' ).
    lo_nodes_exp->add( '/       |b     |bool   |false ||0' ).

    li_writer->set_boolean(
      iv_path = '/a'
      iv_val  = abap_true ).
    li_writer->set_boolean(
      iv_path = '/b'
      iv_val  = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " int
    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |      ||2' ).
    lo_nodes_exp->add( '/       |a     |bool   |true  ||0' ).
    lo_nodes_exp->add( '/       |b     |bool   |false ||0' ).

    li_writer->set_boolean(
      iv_path = '/a'
      iv_val  = 1 ).
    li_writer->set_boolean(
      iv_path = '/b'
      iv_val  = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " tab
    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |      ||2' ).
    lo_nodes_exp->add( '/       |a     |bool   |true  ||0' ).
    lo_nodes_exp->add( '/       |b     |bool   |false ||0' ).

    append 'hello' to lt_tab.
    li_writer->set_boolean(
      iv_path = '/a'
      iv_val  = lt_tab ).
    clear lt_tab.
    li_writer->set_boolean(
      iv_path = '/b'
      iv_val  = lt_tab ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  endmethod.

  method set_str.

    data lo_cut type ref to zcl_ajson.
    data lo_nodes_exp type ref to lcl_nodes_helper.
    data li_writer type ref to zif_ajson.
    data lv_date type d.

    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |         ||3' ).
    lo_nodes_exp->add( '/       |a     |str    |123      ||0' ).
    lo_nodes_exp->add( '/       |b     |str    |X        ||0' ).
    lo_nodes_exp->add( '/       |c     |str    |20200705 ||0' ).

    li_writer->set_string(
      iv_path = '/a'
      iv_val  = '123' ).
    li_writer->set_string(
      iv_path = '/b'
      iv_val  = abap_true ).
    lv_date = '20200705'.
    li_writer->set_string(
      iv_path = '/c'
      iv_val  = lv_date ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  endmethod.

  method set_int.

    data lo_cut type ref to zcl_ajson.
    data lo_nodes_exp type ref to lcl_nodes_helper.
    data li_writer type ref to zif_ajson.

    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |         ||1' ).
    lo_nodes_exp->add( '/       |a     |num    |123      ||0' ).

    li_writer->set_integer(
      iv_path = '/a'
      iv_val  = 123 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  endmethod.

  method set_date.

    data lo_cut type ref to zcl_ajson.
    data lo_nodes_exp type ref to lcl_nodes_helper.
    data li_writer type ref to zif_ajson.
    data lv_date type d.

    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |           ||1' ).
    lo_nodes_exp->add( '/       |a     |str    |2020-07-05 ||0' ).

    lv_date = '20200705'.
    li_writer->set_date(
      iv_path = '/a'
      iv_val  = lv_date ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  endmethod.

  method set_timestamp.

    data lo_cut type ref to zcl_ajson.
    data lo_nodes_exp type ref to lcl_nodes_helper.
    data li_writer type ref to zif_ajson.
    data lv_timestamp type timestamp.

    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |                     ||1' ).
    lo_nodes_exp->add( '/       |a     |str    |2021-05-05T12-00-00Z ||0' ).

    lv_timestamp = '20210505120000'.
    li_writer->set_timestamp(
      iv_path = '/a'
      iv_val  = lv_timestamp ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  endmethod.

  method read_only.

    data lo_cut type ref to zcl_ajson.
    data li_writer type ref to zif_ajson.

    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.

    " Prepare source
    li_writer->set(
      iv_path = '/a'
      iv_val  = 'abc' ).
    li_writer->touch_array( iv_path = '/b' ).
    li_writer->push(
      iv_path = '/b'
      iv_val  = 'abc' ).

    lo_cut->freeze( ).

    try.
      li_writer->set(
        iv_path = '/c'
        iv_val  = 'abc' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error.
    endtry.

    try.
      li_writer->touch_array( iv_path = '/d' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error.
    endtry.

    try.
      li_writer->push(
        iv_path = '/b'
        iv_val  = 'xyz' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error.
    endtry.

    try.
      li_writer->delete( iv_path = '/a' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error.
    endtry.

    try.
      li_writer->clear( ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error.
    endtry.

  endmethod.

  method set_array_obj.

    data lo_cut type ref to zcl_ajson.
    data lo_nodes_exp type ref to lcl_nodes_helper.
    data li_writer type ref to zif_ajson.

    create object lo_nodes_exp.
    lo_nodes_exp->add( '                 |         |object |                        |  |1' ).
    lo_nodes_exp->add( '/                |issues   |array  |                        |  |2' ).
    lo_nodes_exp->add( '/issues/         |1        |object |                        |1 |1' ).
    lo_nodes_exp->add( '/issues/         |2        |object |                        |2 |1' ).
    lo_nodes_exp->add( '/issues/1/       |end      |object |                        |  |2' ).
    lo_nodes_exp->add( '/issues/1/end/   |col      |num    |26                      |  |0' ).
    lo_nodes_exp->add( '/issues/1/end/   |row      |num    |4                       |  |0' ).
    lo_nodes_exp->add( '/issues/2/       |end      |object |                        |  |2' ).
    lo_nodes_exp->add( '/issues/2/end/   |col      |num    |22                      |  |0' ).
    lo_nodes_exp->add( '/issues/2/end/   |row      |num    |3                       |  |0' ).

    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.

    li_writer->touch_array( iv_path = '/issues' ).
    li_writer->set(
      iv_path = '/issues/1/end/col'
      iv_val  = 26 ).
    li_writer->set(
      iv_path = '/issues/1/end/row'
      iv_val  = 4 ).
    li_writer->set(
      iv_path = '/issues/2/end/col'
      iv_val  = 22 ).
    li_writer->set(
      iv_path = '/issues/2/end/row'
      iv_val  = 3 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  endmethod.

  method set_with_type.

    data lo_sample type ref to zcl_ajson.
    data lo_cut type ref to zcl_ajson.
    data li_writer type ref to zif_ajson.

    lo_sample = zcl_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.

    set_with_type_slice( io_json_in  = lo_sample
                         io_json_out = li_writer
                         iv_path     = '/' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_sample->mt_json_tree ).

  endmethod.

  method set_with_type_slice.

    data lv_path type string.

    field-symbols <node> type zif_ajson=>ty_node.

    loop at io_json_in->mt_json_tree assigning <node> where path = iv_path.
      lv_path = <node>-path && <node>-name && '/'.
      case <node>-type.
        when 'array'.
          io_json_out->touch_array( lv_path ).
          set_with_type_slice( io_json_in  = io_json_in
                               io_json_out = io_json_out
                               iv_path     = lv_path ).
        when 'object'.
          set_with_type_slice( io_json_in  = io_json_in
                               io_json_out = io_json_out
                               iv_path     = lv_path ).
        when others.
          io_json_out->set(
            iv_path      = lv_path
            iv_val       = <node>-value
            iv_node_type = <node>-type ).
      endcase.
    endloop.

  endmethod.
endclass.


**********************************************************************
* INTEGRATED
**********************************************************************
class ltcl_integrated definition
  for testing
  risk level harmless
  duration short
  final.

  private section.

    types:
      begin of ty_loc,
        row type i,
        col type i,
      end of ty_loc,
      begin of ty_issue,
        message type string,
        key type string,
        filename type string,
        start type ty_loc,
        end type ty_loc,
      end of ty_issue,
      tt_issues type standard table of ty_issue with default key,
      begin of ty_target,
        string type string,
        number type i,
        float type f,
        boolean type abap_bool,
        false type abap_bool,
        null type string,
        date type string, " ??? TODO
        issues type tt_issues,
      end of ty_target.

    methods reader for testing raising zcx_ajson_error.
    methods array_index for testing raising zcx_ajson_error.
    methods array_simple for testing raising zcx_ajson_error.
    methods stringify for testing raising zcx_ajson_error.
    methods item_order_integrated for testing raising zcx_ajson_error.
    methods chaining for testing raising zcx_ajson_error.

endclass.

class ltcl_integrated implementation.

  method array_simple.

    data lt_act type string_table.
    data lt_exp type string_table.
    data lv_exp type string.

    data lv_src type string.
    lv_src = '['.
    do 10 times.
      if sy-index <> 1.
        lv_src = lv_src && `, `.
      endif.
      lv_src = lv_src && |"{ sy-index }"|.
      lv_exp = |{ sy-index }|.
      append lv_exp to lt_exp.
    enddo.
    lv_src = lv_src && ']'.

    data li_reader type ref to zif_ajson.
    li_reader = zcl_ajson=>parse( lv_src ).
    li_reader->to_abap( importing ev_container = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  endmethod.

  method array_index.

    data lt_act type table of ty_loc.
    data lt_exp type table of ty_loc.
    data ls_exp type ty_loc.

    data lv_src type string.
    lv_src = '['.
    do 10 times.
      if sy-index <> 1.
        lv_src = lv_src && `, `.
      endif.
      lv_src = lv_src && |\{ "row": { sy-index } \}|.
      ls_exp-row = sy-index.
      append ls_exp to lt_exp.
    enddo.
    lv_src = lv_src && ']'.

    data li_reader type ref to zif_ajson.
    li_reader = zcl_ajson=>parse( lv_src ).
    li_reader->to_abap( importing ev_container = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  endmethod.

  method reader.

    data lv_source type string.
    data li_reader type ref to zif_ajson.

    lv_source = ltcl_parser_test=>sample_json( ).
    li_reader = zcl_ajson=>parse( lv_source ).

    cl_abap_unit_assert=>assert_equals(
      act = li_reader->get( '/string' )
      exp = 'abc' ).

    data ls_act type ty_target.
    data ls_exp type ty_target.
    field-symbols <i> like line of ls_exp-issues.

    ls_exp-string = 'abc'.
    ls_exp-number = 123.
    ls_exp-float = '123.45'.
    ls_exp-boolean = abap_true.
    ls_exp-false = abap_false.
    ls_exp-date = '2020-03-15'.

    append initial line to ls_exp-issues assigning <i>.
    <i>-message  = 'Indentation problem ...'.
    <i>-key      = 'indentation'.
    <i>-filename = './zxxx.prog.abap'.
    <i>-start-row = 4.
    <i>-start-col = 3.
    <i>-end-row   = 4.
    <i>-end-col   = 26.

    append initial line to ls_exp-issues assigning <i>.
    <i>-message  = 'Remove space before XXX'.
    <i>-key      = 'space_before_dot'.
    <i>-filename = './zxxx.prog.abap'.
    <i>-start-row = 3.
    <i>-start-col = 21.
    <i>-end-row   = 3.
    <i>-end-col   = 22.

    li_reader->to_abap( importing ev_container = ls_act ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_act
      exp = ls_exp ).

  endmethod.

  method stringify.

    data lo_cut type ref to zcl_ajson.
    data li_writer type ref to zif_ajson.
    data lv_exp type string.
    data: begin of ls_dummy, x type i, end of ls_dummy.

    ls_dummy-x = 1.
    lo_cut    = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.

    li_writer->set(
      iv_path = '/a'
      iv_val  = 1 ).
    li_writer->set(
      iv_path = '/b'
      iv_val  = 'B' ).
    li_writer->set(
      iv_path = '/c'
      iv_val  = abap_true ).
    li_writer->set_null( iv_path = '/d' ).

    " simple test
    lv_exp = '{"a":1,"b":"B","c":true,"d":null}'.
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->stringify( )
      exp = lv_exp ).

    li_writer->touch_array( iv_path = '/e' ).
    li_writer->touch_array( iv_path = '/f' ).
    li_writer->push(
      iv_path = '/f'
      iv_val  = 5 ).
    li_writer->push(
      iv_path = '/f'
      iv_val  = ls_dummy ).
    li_writer->set(
      iv_path = '/g'
      iv_val  = ls_dummy ).

    " complex test
    lv_exp = '{"a":1,"b":"B","c":true,"d":null,"e":[],"f":[5,{"x":1}],"g":{"x":1}}'.
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->stringify( )
      exp = lv_exp ).

    " complex test indented
    lv_exp =
      '{\n' &&
      '  "a": 1,\n' &&
      '  "b": "B",\n' &&
      '  "c": true,\n' &&
      '  "d": null,\n' &&
      '  "e": [],\n' &&
      '  "f": [\n' &&
      '    5,\n' &&
      '    {\n' &&
      '      "x": 1\n' &&
      '    }\n' &&
      '  ],\n' &&
      '  "g": {\n' &&
      '    "x": 1\n' &&
      '  }\n' &&
      '}'.
    lv_exp = replace(
      val = lv_exp
      sub = '\n'
      with = cl_abap_char_utilities=>newline
      occ = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->stringify( iv_indent = 2 )
      exp = lv_exp ).

  endmethod.

  method item_order_integrated.

    data:
      begin of ls_dummy,
        zulu type string,
        alpha type string,
        beta type string,
      end of ls_dummy.

    data lv_act type string.
    data lv_exp type string.
    data li_cut type ref to zif_ajson.

    ls_dummy-alpha = 'a'.
    ls_dummy-beta  = 'b'.
    ls_dummy-zulu  = 'z'.

    " NAME order
    li_cut = zcl_ajson=>create_empty( ).
    li_cut->set(
      iv_path = '/'
      iv_val  = ls_dummy ).

    lv_act = li_cut->stringify( ).
    lv_exp = '{"alpha":"a","beta":"b","zulu":"z"}'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

    " STRUC order (keep)
    li_cut = zcl_ajson=>create_empty( ).
    li_cut->keep_item_order( ).
    li_cut->set(
      iv_path = '/'
      iv_val  = ls_dummy ).

    lv_act = li_cut->stringify( ).
    lv_exp = '{"zulu":"z","alpha":"a","beta":"b"}'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  endmethod.

  method chaining.

    data li_cut type ref to zif_ajson.

    li_cut = zcl_ajson=>create_empty( ).

    cl_abap_unit_assert=>assert_bound(
      li_cut->set(
        iv_path = '/a'
        iv_val  = 1 ) ).

    cl_abap_unit_assert=>assert_bound( li_cut->delete( iv_path = '/a' ) ).

    cl_abap_unit_assert=>assert_bound( li_cut->touch_array( iv_path = '/array' ) ).

    cl_abap_unit_assert=>assert_bound(
      li_cut->push(
        iv_path = '/array'
        iv_val  = '1' ) ).

    cl_abap_unit_assert=>assert_bound( li_cut->keep_item_order( ) ).

  endmethod.

endclass.

**********************************************************************
* ABAP TO JSON
**********************************************************************
class ltcl_abap_to_json definition
  for testing
  risk level harmless
  duration short
  final.

  private section.

    types:
      begin of ty_struc,
        a type string,
        b type i,
        c type abap_bool,
        d type xfeld,
      end of ty_struc,
      tt_struc type standard table of ty_struc with default key,
      begin of ty_struc_complex.
        include type ty_struc.
    types:
        el type string,
        struc type ty_struc,
        tab type tt_struc,
        stab type string_table,
      end of ty_struc_complex.

    methods set_ajson for testing raising zcx_ajson_error.
    methods set_value for testing raising zcx_ajson_error.
    methods set_null for testing raising zcx_ajson_error.
    methods set_obj for testing raising zcx_ajson_error.
    methods set_array for testing raising zcx_ajson_error.
    methods set_complex_obj for testing raising zcx_ajson_error.
    methods prefix for testing raising zcx_ajson_error.

endclass.

class zcl_ajson definition local friends ltcl_abap_to_json.

class ltcl_abap_to_json implementation.

  method set_ajson.

    data lo_nodes type ref to lcl_nodes_helper.
    data lo_src type ref to zcl_ajson.
    lo_src = zcl_ajson=>create_empty( ).

    create object lo_nodes.
    lo_nodes->add( '        |      |object |     ||1' ).
    lo_nodes->add( '/       |a     |object |     ||1' ).
    lo_nodes->add( '/a/     |b     |object |     ||1' ).
    lo_nodes->add( '/a/b/   |c     |object |     ||0' ).
    lo_src->mt_json_tree = lo_nodes->mt_nodes.

    data lt_nodes type zif_ajson=>ty_nodes_tt.
    lt_nodes = lcl_abap_to_json=>convert( iv_data = lo_src ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes->mt_nodes ).

  endmethod.

  method set_value.

    data lo_nodes_exp type ref to lcl_nodes_helper.
    data lt_nodes type zif_ajson=>ty_nodes_tt.

    " number
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |num |1     ||' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

    " string
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |str |abc     ||' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = 'abc' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

    " true
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |bool |true     ||' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

    " false
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |bool |false    ||' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

    " xfeld
    data lv_xfeld type xfeld.
    create object lo_nodes_exp.
    lo_nodes_exp->add( '        |      |bool |true     ||' ).

    lv_xfeld = 'X'.
    lt_nodes = lcl_abap_to_json=>convert( iv_data = lv_xfeld ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

  endmethod.

  method set_null.

    data lo_nodes_exp type ref to lcl_nodes_helper.
    data lt_nodes type zif_ajson=>ty_nodes_tt.
    data lv_null_ref type ref to data.

    " null
    create object lo_nodes_exp.
    lo_nodes_exp->add( '       |      |null |null ||' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = lv_null_ref ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

  endmethod.

  method prefix.

    data lo_nodes_exp type ref to lcl_nodes_helper.
    data lt_nodes type zif_ajson=>ty_nodes_tt.
    data ls_prefix type zif_ajson=>ty_path_name.

    ls_prefix-path = '/a/'.
    ls_prefix-name = 'b'.
    create object lo_nodes_exp.
    lo_nodes_exp->add( '/a/       |b     |num |1     ||' ).

    lt_nodes = lcl_abap_to_json=>convert(
      iv_data   = 1
      is_prefix = ls_prefix ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

  endmethod.

  method set_obj.

    data lo_nodes_exp type ref to lcl_nodes_helper.
    data ls_struc type ty_struc.
    data lt_nodes type zif_ajson=>ty_nodes_tt.

    ls_struc-a = 'abc'.
    ls_struc-b = 10.
    ls_struc-c = abap_true.
    ls_struc-d = 'X'.

    create object lo_nodes_exp.
    lo_nodes_exp->add( '       |      |object |     ||4' ).
    lo_nodes_exp->add( '/      |a     |str    |abc  ||0' ).
    lo_nodes_exp->add( '/      |b     |num    |10   ||0' ).
    lo_nodes_exp->add( '/      |c     |bool   |true ||0' ).
    lo_nodes_exp->add( '/      |d     |bool   |true ||0' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = ls_struc ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

  endmethod.

  method set_complex_obj.

    data lo_nodes_exp type ref to lcl_nodes_helper.
    data ls_struc type ty_struc_complex.
    data lt_nodes type zif_ajson=>ty_nodes_tt.
    field-symbols <i> like line of ls_struc-tab.

    ls_struc-a = 'abc'.
    ls_struc-b = 10.
    ls_struc-c = abap_true.
    ls_struc-d = 'X'.
    ls_struc-el = 'elem'.

    ls_struc-struc-a = 'deep'.
    ls_struc-struc-b = 123.

    append 'hello' to ls_struc-stab.
    append 'world' to ls_struc-stab.

    append initial line to ls_struc-tab assigning <i>.
    <i>-a = 'abc'.
    append initial line to ls_struc-tab assigning <i>.
    <i>-a = 'bcd'.

    create object lo_nodes_exp.
    lo_nodes_exp->add( '       |      |object |     ||8' ).
    lo_nodes_exp->add( '/      |a     |str    |abc  ||0' ).
    lo_nodes_exp->add( '/      |b     |num    |10   ||0' ).
    lo_nodes_exp->add( '/      |c     |bool   |true ||0' ).
    lo_nodes_exp->add( '/      |d     |bool   |true ||0' ).
    lo_nodes_exp->add( '/      |el    |str    |elem ||0' ).
    lo_nodes_exp->add( '/      |struc |object |     ||4' ).
    lo_nodes_exp->add( '/struc/|a     |str    |deep ||0' ).
    lo_nodes_exp->add( '/struc/|b     |num    |123  ||0' ).
    lo_nodes_exp->add( '/struc/|c     |bool   |false||0' ).
    lo_nodes_exp->add( '/struc/|d     |bool   |false||0' ).

    lo_nodes_exp->add( '/      |tab   |array  |     | |2' ).
    lo_nodes_exp->add( '/tab/  |1     |object |     |1|4' ).
    lo_nodes_exp->add( '/tab/1/|a     |str    |abc  | |0' ).
    lo_nodes_exp->add( '/tab/1/|b     |num    |0    | |0' ).
    lo_nodes_exp->add( '/tab/1/|c     |bool   |false| |0' ).
    lo_nodes_exp->add( '/tab/1/|d     |bool   |false| |0' ).
    lo_nodes_exp->add( '/tab/  |2     |object |     |2|4' ).
    lo_nodes_exp->add( '/tab/2/|a     |str    |bcd  | |0' ).
    lo_nodes_exp->add( '/tab/2/|b     |num    |0    | |0' ).
    lo_nodes_exp->add( '/tab/2/|c     |bool   |false| |0' ).
    lo_nodes_exp->add( '/tab/2/|d     |bool   |false| |0' ).

    lo_nodes_exp->add( '/      |stab  |array  |     | |2' ).
    lo_nodes_exp->add( '/stab/ |1     |str    |hello|1|0' ).
    lo_nodes_exp->add( '/stab/ |2     |str    |world|2|0' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = ls_struc ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

  endmethod.

  method set_array.

    data lo_nodes_exp type ref to lcl_nodes_helper.
    data lt_nodes type zif_ajson=>ty_nodes_tt.

    data lt_tab type table of ty_struc.
    field-symbols <s> like line of lt_tab.

    append initial line to lt_tab assigning <s>.
    <s>-a = 'abc'.
    <s>-b = 10.
    append initial line to lt_tab assigning <s>.
    <s>-a = 'bcd'.
    <s>-b = 20.

    create object lo_nodes_exp.
    lo_nodes_exp->add( '       |      |array  |     | |2' ).
    lo_nodes_exp->add( '/      |1     |object |     |1|4' ).
    lo_nodes_exp->add( '/1/    |a     |str    |abc  | |0' ).
    lo_nodes_exp->add( '/1/    |b     |num    |10   | |0' ).
    lo_nodes_exp->add( '/1/    |c     |bool   |false| |0' ).
    lo_nodes_exp->add( '/1/    |d     |bool   |false| |0' ).
    lo_nodes_exp->add( '/      |2     |object |     |2|4' ).
    lo_nodes_exp->add( '/2/    |a     |str    |bcd  | |0' ).
    lo_nodes_exp->add( '/2/    |b     |num    |20   | |0' ).
    lo_nodes_exp->add( '/2/    |c     |bool   |false| |0' ).
    lo_nodes_exp->add( '/2/    |d     |bool   |false| |0' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = lt_tab ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

    data lt_strtab type string_table.
    append 'abc' to lt_strtab.
    append 'bcd' to lt_strtab.

    create object lo_nodes_exp.
    lo_nodes_exp->add( '       |      |array  |     | |2' ).
    lo_nodes_exp->add( '/      |1     |str    |abc  |1|0' ).
    lo_nodes_exp->add( '/      |2     |str    |bcd  |2|0' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = lt_strtab ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

  endmethod.

endclass.
