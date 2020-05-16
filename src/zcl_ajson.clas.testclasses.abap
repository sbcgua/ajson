class ltcl_parser_test definition final
  for testing
  risk level harmless
  duration short.

  private section.

    class-data gv_sample type string.

    class-methods class_setup.

    methods parse for testing raising zcx_ajson_error.
    methods normalize_path for testing.
    methods split_path for testing.
    methods get_value for testing raising zcx_ajson_error.
    methods exists for testing raising zcx_ajson_error.
    methods value_integer for testing raising zcx_ajson_error.
    methods value_boolean for testing raising zcx_ajson_error.
    methods members for testing raising zcx_ajson_error.
    methods slice for testing raising zcx_ajson_error.

    data mt_exp type zcl_ajson=>ty_nodes_tt.
    methods _exp
      importing
        iv_str type string.

endclass.

class zcl_ajson definition local friends ltcl_parser_test.

class ltcl_parser_test implementation.

  method normalize_path.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>normalize_path( '' )
      exp = '/' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>normalize_path( '/' )
      exp = '/' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>normalize_path( 'abc' )
      exp = '/abc/' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>normalize_path( '/abc' )
      exp = '/abc/' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>normalize_path( 'abc/' )
      exp = '/abc/' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>normalize_path( '/abc/' )
      exp = '/abc/' ).

  endmethod.

  method split_path.

    data ls_exp type zcl_ajson=>ty_path_name.
    data lv_path type string.

    lv_path     = ''. " alias to root
    ls_exp-path = ''.
    ls_exp-name = ''.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = '/'.
    ls_exp-path = ''.
    ls_exp-name = ''.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = '/abc/'.
    ls_exp-path = '/'.
    ls_exp-name = 'abc'.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = 'abc'.
    ls_exp-path = '/'.
    ls_exp-name = 'abc'.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = '/abc'.
    ls_exp-path = '/'.
    ls_exp-name = 'abc'.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = 'abc/'.
    ls_exp-path = '/'.
    ls_exp-name = 'abc'.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = '/abc/xyz'.
    ls_exp-path = '/abc/'.
    ls_exp-name = 'xyz'.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = '/abc/xyz/'.
    ls_exp-path = '/abc/'.
    ls_exp-name = 'xyz'.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_ajson=>split_path( lv_path )
      exp = ls_exp ).

  endmethod.

  method class_setup.

    gv_sample =
      '{' &&
      '  "string": "abc",' &&
      '  "number": 123,' &&
      '  "float": 123.45,' &&
      '  "boolean": true,' &&
      '  "false": false,' &&
      '  "null": null,' &&
      '  "date": "2020-03-15",' &&
      '  "issues": [' &&
      '    {' &&
      '      "message": "Indentation problem ...",' &&
      '      "key": "indentation",' &&
      '      "start": {' &&
      '        "row": 4,' &&
      '        "col": 3' &&
      '      },' &&
      '      "end": {' &&
      '        "row": 4,' &&
      '        "col": 26' &&
      '      },' &&
      '      "filename": "./zxxx.prog.abap"' &&
      '    },' &&
      '    {' &&
      '      "message": "Remove space before XXX",' &&
      '      "key": "space_before_dot",' &&
      '      "start": {' &&
      '        "row": 3,' &&
      '        "col": 21' &&
      '      },' &&
      '      "end": {' &&
      '        "row": 3,' &&
      '        "col": 22' &&
      '      },' &&
      '      "filename": "./zxxx.prog.abap"' &&
      '    }' &&
      '  ]' &&
      '}'.

  endmethod.

  method _exp.

    field-symbols <exp> like line of mt_exp.
    data lv_children type string.

    append initial line to mt_exp assigning <exp>.

    split iv_str at '|' into
      <exp>-path
      <exp>-name
      <exp>-type
      <exp>-value
      lv_children.
    condense <exp>-path.
    condense <exp>-name.
    condense <exp>-type.
    condense <exp>-value.
    <exp>-children = lv_children.

  endmethod.

  method parse.

    data lo_cut type ref to lcl_json_parser.
    data lt_act type zcl_ajson=>ty_nodes_tt.

    clear mt_exp.
    _exp( '                 |         |object |                        |8' ).
    _exp( '/                |string   |str    |abc                     |0' ).
    _exp( '/                |number   |num    |123                     |0' ).
    _exp( '/                |float    |num    |123.45                  |0' ).
    _exp( '/                |boolean  |bool   |true                    |0' ).
    _exp( '/                |false    |bool   |false                   |0' ).
    _exp( '/                |null     |null   |                        |0' ).
    _exp( '/                |date     |str    |2020-03-15              |0' ).
    _exp( '/                |issues   |array  |                        |2' ).
    _exp( '/issues/         |1        |object |                        |5' ).
    _exp( '/issues/1/       |message  |str    |Indentation problem ... |0' ).
    _exp( '/issues/1/       |key      |str    |indentation             |0' ).
    _exp( '/issues/1/       |start    |object |                        |2' ).
    _exp( '/issues/1/start/ |row      |num    |4                       |0' ).
    _exp( '/issues/1/start/ |col      |num    |3                       |0' ).
    _exp( '/issues/1/       |end      |object |                        |2' ).
    _exp( '/issues/1/end/   |row      |num    |4                       |0' ).
    _exp( '/issues/1/end/   |col      |num    |26                      |0' ).
    _exp( '/issues/1/       |filename |str    |./zxxx.prog.abap        |0' ).
    _exp( '/issues/         |2        |object |                        |5' ).
    _exp( '/issues/2/       |message  |str    |Remove space before XXX |0' ).
    _exp( '/issues/2/       |key      |str    |space_before_dot        |0' ).
    _exp( '/issues/2/       |start    |object |                        |2' ).
    _exp( '/issues/2/start/ |row      |num    |3                       |0' ).
    _exp( '/issues/2/start/ |col      |num    |21                      |0' ).
    _exp( '/issues/2/       |end      |object |                        |2' ).
    _exp( '/issues/2/end/   |row      |num    |3                       |0' ).
    _exp( '/issues/2/end/   |col      |num    |22                      |0' ).
    _exp( '/issues/2/       |filename |str    |./zxxx.prog.abap        |0' ).

    create object lo_cut.
    lt_act = lo_cut->parse( gv_sample ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mt_exp ).

  endmethod.

  method slice.

    data lo_cut type ref to zcl_ajson.

    clear mt_exp.
    _exp( '          |         |array  |                        |2' ).
    _exp( '/         |1        |object |                        |5' ).
    _exp( '/1/       |message  |str    |Indentation problem ... |0' ).
    _exp( '/1/       |key      |str    |indentation             |0' ).
    _exp( '/1/       |start    |object |                        |2' ).
    _exp( '/1/start/ |row      |num    |4                       |0' ).
    _exp( '/1/start/ |col      |num    |3                       |0' ).
    _exp( '/1/       |end      |object |                        |2' ).
    _exp( '/1/end/   |row      |num    |4                       |0' ).
    _exp( '/1/end/   |col      |num    |26                      |0' ).
    _exp( '/1/       |filename |str    |./zxxx.prog.abap        |0' ).
    _exp( '/         |2        |object |                        |5' ).
    _exp( '/2/       |message  |str    |Remove space before XXX |0' ).
    _exp( '/2/       |key      |str    |space_before_dot        |0' ).
    _exp( '/2/       |start    |object |                        |2' ).
    _exp( '/2/start/ |row      |num    |3                       |0' ).
    _exp( '/2/start/ |col      |num    |21                      |0' ).
    _exp( '/2/       |end      |object |                        |2' ).
    _exp( '/2/end/   |row      |num    |3                       |0' ).
    _exp( '/2/end/   |col      |num    |22                      |0' ).
    _exp( '/2/       |filename |str    |./zxxx.prog.abap        |0' ).
    sort mt_exp by path name. " Imitate sorted tab

    lo_cut = zcl_ajson=>parse( gv_sample ).
    lo_cut ?= lo_cut->zif_ajson_reader~slice( '/issues' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = mt_exp ).

    " **********************************************************************

    clear mt_exp.
    _exp( '                 |         |object |                        |8' ).
    _exp( '/                |string   |str    |abc                     |0' ).
    _exp( '/                |number   |num    |123                     |0' ).
    _exp( '/                |float    |num    |123.45                  |0' ).
    _exp( '/                |boolean  |bool   |true                    |0' ).
    _exp( '/                |false    |bool   |false                   |0' ).
    _exp( '/                |null     |null   |                        |0' ).
    _exp( '/                |date     |str    |2020-03-15              |0' ).
    _exp( '/                |issues   |array  |                        |2' ).
    _exp( '/issues/         |1        |object |                        |5' ).
    _exp( '/issues/1/       |message  |str    |Indentation problem ... |0' ).
    _exp( '/issues/1/       |key      |str    |indentation             |0' ).
    _exp( '/issues/1/       |start    |object |                        |2' ).
    _exp( '/issues/1/start/ |row      |num    |4                       |0' ).
    _exp( '/issues/1/start/ |col      |num    |3                       |0' ).
    _exp( '/issues/1/       |end      |object |                        |2' ).
    _exp( '/issues/1/end/   |row      |num    |4                       |0' ).
    _exp( '/issues/1/end/   |col      |num    |26                      |0' ).
    _exp( '/issues/1/       |filename |str    |./zxxx.prog.abap        |0' ).
    _exp( '/issues/         |2        |object |                        |5' ).
    _exp( '/issues/2/       |message  |str    |Remove space before XXX |0' ).
    _exp( '/issues/2/       |key      |str    |space_before_dot        |0' ).
    _exp( '/issues/2/       |start    |object |                        |2' ).
    _exp( '/issues/2/start/ |row      |num    |3                       |0' ).
    _exp( '/issues/2/start/ |col      |num    |21                      |0' ).
    _exp( '/issues/2/       |end      |object |                        |2' ).
    _exp( '/issues/2/end/   |row      |num    |3                       |0' ).
    _exp( '/issues/2/end/   |col      |num    |22                      |0' ).
    _exp( '/issues/2/       |filename |str    |./zxxx.prog.abap        |0' ).
    sort mt_exp by path name. " Imitate sorted tab

    lo_cut = zcl_ajson=>parse( gv_sample ).
    lo_cut ?= lo_cut->zif_ajson_reader~slice( '/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = mt_exp ).

    " **********************************************************************

    clear mt_exp.
    _exp( '  |         |object |                        |2' ).
    _exp( '/ |row      |num    |3                       |0' ).
    _exp( '/ |col      |num    |21                      |0' ).
    sort mt_exp by path name. " Imitate sorted tab

    lo_cut = zcl_ajson=>parse( gv_sample ).
    lo_cut ?= lo_cut->zif_ajson_reader~slice( '/issues/2/start/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = mt_exp ).

  endmethod.

  method get_value.

    data lo_cut type ref to zif_ajson_reader.
    lo_cut ?= zcl_ajson=>parse( gv_sample ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value( '/string' )
      exp = 'abc' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value( '/string/' )
      exp = 'abc' ). " Hmmm ?

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value( '/boolean' )
      exp = 'true' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value( '/issues/2/start/row' )
      exp = '3' ).

  endmethod.

  method exists.

    data lo_cut type ref to zif_ajson_reader.
    lo_cut ?= zcl_ajson=>parse( gv_sample ).


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

    data lo_cut type ref to zif_ajson_reader.
    lo_cut ?= zcl_ajson=>parse( gv_sample ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value_integer( '/string' )
      exp = 0 ). " Hmmmm ????

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value_integer( '/number' )
      exp = 123 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value_integer( '/float' )
      exp = 123 ).

  endmethod.

  method value_boolean.

    data lo_cut type ref to zif_ajson_reader.
    lo_cut ?= zcl_ajson=>parse( gv_sample ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value_boolean( '/string' )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value_boolean( '/number' )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value_boolean( '/xxx' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value_boolean( '/boolean' )
      exp = abap_true ).

  endmethod.

  method members.

    data lt_exp type string_table.
    data lo_cut type ref to zif_ajson_reader.
    lo_cut ?= zcl_ajson=>parse( gv_sample ).

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

endclass.
