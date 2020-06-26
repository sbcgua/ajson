

**********************************************************************
* UTIL
**********************************************************************
class lcl_nodes_helper definition final.
  public section.

    data mt_nodes type zcl_ajson=>ty_nodes_tt.
    methods add
      importing
        iv_str type string.
    methods sorted
      returning
        value(rt_nodes) type zcl_ajson=>ty_nodes_ts.

endclass.

class lcl_nodes_helper implementation.
  method add.

    field-symbols <n> like line of mt_nodes.
    data lv_children type string.
    data lv_index type string.

    append initial line to mt_nodes assigning <n>.

    split iv_str at '|' into
      <n>-path
      <n>-name
      <n>-type
      <n>-value
      lv_index
      lv_children.
    condense <n>-path.
    condense <n>-name.
    condense <n>-type.
    condense <n>-value.
    <n>-index = lv_index.
    <n>-children = lv_children.

  endmethod.
  method sorted.
    rt_nodes = mt_nodes.
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
      returning
        value(rv_json) type string.

  private section.

    methods parse for testing raising zcx_ajson_error.

endclass.

class zcl_ajson definition local friends ltcl_parser_test.

class ltcl_parser_test implementation.

  method sample_json.

    rv_json =
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

  method parse.

    data lo_cut type ref to lcl_json_parser.
    data lt_act type zcl_ajson=>ty_nodes_tt.
    data nodes type ref to lcl_nodes_helper.

    create object nodes.
    nodes->add( '                 |         |object |                        |  |8' ).
    nodes->add( '/                |string   |str    |abc                     |  |0' ).
    nodes->add( '/                |number   |num    |123                     |  |0' ).
    nodes->add( '/                |float    |num    |123.45                  |  |0' ).
    nodes->add( '/                |boolean  |bool   |true                    |  |0' ).
    nodes->add( '/                |false    |bool   |false                   |  |0' ).
    nodes->add( '/                |null     |null   |                        |  |0' ).
    nodes->add( '/                |date     |str    |2020-03-15              |  |0' ).
    nodes->add( '/                |issues   |array  |                        |  |2' ).
    nodes->add( '/issues/         |1        |object |                        |1 |5' ).
    nodes->add( '/issues/1/       |message  |str    |Indentation problem ... |  |0' ).
    nodes->add( '/issues/1/       |key      |str    |indentation             |  |0' ).
    nodes->add( '/issues/1/       |start    |object |                        |  |2' ).
    nodes->add( '/issues/1/start/ |row      |num    |4                       |  |0' ).
    nodes->add( '/issues/1/start/ |col      |num    |3                       |  |0' ).
    nodes->add( '/issues/1/       |end      |object |                        |  |2' ).
    nodes->add( '/issues/1/end/   |row      |num    |4                       |  |0' ).
    nodes->add( '/issues/1/end/   |col      |num    |26                      |  |0' ).
    nodes->add( '/issues/1/       |filename |str    |./zxxx.prog.abap        |  |0' ).
    nodes->add( '/issues/         |2        |object |                        |2 |5' ).
    nodes->add( '/issues/2/       |message  |str    |Remove space before XXX |  |0' ).
    nodes->add( '/issues/2/       |key      |str    |space_before_dot        |  |0' ).
    nodes->add( '/issues/2/       |start    |object |                        |  |2' ).
    nodes->add( '/issues/2/start/ |row      |num    |3                       |  |0' ).
    nodes->add( '/issues/2/start/ |col      |num    |21                      |  |0' ).
    nodes->add( '/issues/2/       |end      |object |                        |  |2' ).
    nodes->add( '/issues/2/end/   |row      |num    |3                       |  |0' ).
    nodes->add( '/issues/2/end/   |col      |num    |22                      |  |0' ).
    nodes->add( '/issues/2/       |filename |str    |./zxxx.prog.abap        |  |0' ).

    create object lo_cut.
    lt_act = lo_cut->parse( sample_json( ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = nodes->mt_nodes ).

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

endclass.

class zcl_ajson definition local friends ltcl_utils_test.

class ltcl_utils_test implementation.

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
    methods exists for testing raising zcx_ajson_error.
    methods value_integer for testing raising zcx_ajson_error.
    methods value_boolean for testing raising zcx_ajson_error.
    methods members for testing raising zcx_ajson_error.
    methods slice for testing raising zcx_ajson_error.

endclass.

class zcl_ajson definition local friends ltcl_reader_test.

class ltcl_reader_test implementation.

  method slice.

    data lo_cut type ref to zcl_ajson.
    data nodes type ref to lcl_nodes_helper.

    create object nodes.
    nodes->add( '          |         |array  |                        |  |2' ).
    nodes->add( '/         |1        |object |                        |1 |5' ).
    nodes->add( '/1/       |message  |str    |Indentation problem ... |  |0' ).
    nodes->add( '/1/       |key      |str    |indentation             |  |0' ).
    nodes->add( '/1/       |start    |object |                        |  |2' ).
    nodes->add( '/1/start/ |row      |num    |4                       |  |0' ).
    nodes->add( '/1/start/ |col      |num    |3                       |  |0' ).
    nodes->add( '/1/       |end      |object |                        |  |2' ).
    nodes->add( '/1/end/   |row      |num    |4                       |  |0' ).
    nodes->add( '/1/end/   |col      |num    |26                      |  |0' ).
    nodes->add( '/1/       |filename |str    |./zxxx.prog.abap        |  |0' ).
    nodes->add( '/         |2        |object |                        |2 |5' ).
    nodes->add( '/2/       |message  |str    |Remove space before XXX |  |0' ).
    nodes->add( '/2/       |key      |str    |space_before_dot        |  |0' ).
    nodes->add( '/2/       |start    |object |                        |  |2' ).
    nodes->add( '/2/start/ |row      |num    |3                       |  |0' ).
    nodes->add( '/2/start/ |col      |num    |21                      |  |0' ).
    nodes->add( '/2/       |end      |object |                        |  |2' ).
    nodes->add( '/2/end/   |row      |num    |3                       |  |0' ).
    nodes->add( '/2/end/   |col      |num    |22                      |  |0' ).
    nodes->add( '/2/       |filename |str    |./zxxx.prog.abap        |  |0' ).


    lo_cut = zcl_ajson=>parse( ltcl_parser_test=>sample_json( ) ).
    lo_cut ?= lo_cut->zif_ajson_reader~slice( '/issues' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = nodes->sorted( ) ).

    " **********************************************************************

    create object nodes.
    nodes->add( '                 |         |object |                        |  |8' ).
    nodes->add( '/                |string   |str    |abc                     |  |0' ).
    nodes->add( '/                |number   |num    |123                     |  |0' ).
    nodes->add( '/                |float    |num    |123.45                  |  |0' ).
    nodes->add( '/                |boolean  |bool   |true                    |  |0' ).
    nodes->add( '/                |false    |bool   |false                   |  |0' ).
    nodes->add( '/                |null     |null   |                        |  |0' ).
    nodes->add( '/                |date     |str    |2020-03-15              |  |0' ).
    nodes->add( '/                |issues   |array  |                        |  |2' ).
    nodes->add( '/issues/         |1        |object |                        |1 |5' ).
    nodes->add( '/issues/1/       |message  |str    |Indentation problem ... |  |0' ).
    nodes->add( '/issues/1/       |key      |str    |indentation             |  |0' ).
    nodes->add( '/issues/1/       |start    |object |                        |  |2' ).
    nodes->add( '/issues/1/start/ |row      |num    |4                       |  |0' ).
    nodes->add( '/issues/1/start/ |col      |num    |3                       |  |0' ).
    nodes->add( '/issues/1/       |end      |object |                        |  |2' ).
    nodes->add( '/issues/1/end/   |row      |num    |4                       |  |0' ).
    nodes->add( '/issues/1/end/   |col      |num    |26                      |  |0' ).
    nodes->add( '/issues/1/       |filename |str    |./zxxx.prog.abap        |  |0' ).
    nodes->add( '/issues/         |2        |object |                        |2 |5' ).
    nodes->add( '/issues/2/       |message  |str    |Remove space before XXX |  |0' ).
    nodes->add( '/issues/2/       |key      |str    |space_before_dot        |  |0' ).
    nodes->add( '/issues/2/       |start    |object |                        |  |2' ).
    nodes->add( '/issues/2/start/ |row      |num    |3                       |  |0' ).
    nodes->add( '/issues/2/start/ |col      |num    |21                      |  |0' ).
    nodes->add( '/issues/2/       |end      |object |                        |  |2' ).
    nodes->add( '/issues/2/end/   |row      |num    |3                       |  |0' ).
    nodes->add( '/issues/2/end/   |col      |num    |22                      |  |0' ).
    nodes->add( '/issues/2/       |filename |str    |./zxxx.prog.abap        |  |0' ).

    lo_cut = zcl_ajson=>parse( ltcl_parser_test=>sample_json( ) ).
    lo_cut ?= lo_cut->zif_ajson_reader~slice( '/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = nodes->sorted( ) ).

    " **********************************************************************

    create object nodes.
    nodes->add( '  |         |object |                        | |2' ).
    nodes->add( '/ |row      |num    |3                       | |0' ).
    nodes->add( '/ |col      |num    |21                      | |0' ).

    lo_cut = zcl_ajson=>parse( ltcl_parser_test=>sample_json( ) ).
    lo_cut ?= lo_cut->zif_ajson_reader~slice( '/issues/2/start/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = nodes->sorted( ) ).

  endmethod.

  method get_value.

    data lo_cut type ref to zif_ajson_reader.
    lo_cut ?= zcl_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

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

    data lo_cut type ref to zif_ajson_reader.
    lo_cut ?= zcl_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

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
    lo_cut ?= zcl_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

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
        str type string,
        int type i,
        float type f,
        bool type abap_bool,
        obj type ty_struc,
        tab type tty_struc,
        oref type ref to object,
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
    data mock type ty_complex.
    data lo_cut type ref to lcl_json_to_abap.

    prepare_cut(
      importing
        eo_cut = lo_cut
        e_mock = mock
        e_elem = last_elem ).

    data ref type ref to data.
    field-symbols <val> type any.

    ref = lo_cut->find_loc( 'str' ). " Relative also works but from root
    assign ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 'Hello' ).

    ref = lo_cut->find_loc( '/str' ).
    assign ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 'Hello' ).

    ref = lo_cut->find_loc( '/int' ).
    assign ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 10 ).

    ref = lo_cut->find_loc( '/obj/a' ).
    assign ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 'World' ).

    ref = lo_cut->find_loc( iv_path = '/obj' iv_name = 'a' ).
    assign ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 'World' ).

    ref = lo_cut->find_loc( '/obj' ).
    assign ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = mock-obj ).

    ref = lo_cut->find_loc( '/' ).
    assign ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = mock ).

    ref = lo_cut->find_loc( '/tab/2' ).
    assign ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = last_elem ).

    ref = lo_cut->find_loc( '/tab/1/a' ).
    assign ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 'One' ).

  endmethod.

  method find_loc_append.

    data last_elem type ty_struc.
    data mock type ty_complex.
    data lo_cut type ref to lcl_json_to_abap.
    data lx type ref to zcx_ajson_error.

    prepare_cut(
      importing
        eo_cut = lo_cut
        e_mock = mock
        e_elem = last_elem ).

    data ref type ref to data.
    field-symbols <val> type any.

    ref = lo_cut->find_loc( '/tab/1/a' ).
    assign ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 'One' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mock-tab )
      exp = 2 ).

    try.
      lo_cut->find_loc( '/tab/3/a' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Index not found in table' ).
    endtry.

    ref = lo_cut->find_loc( iv_path = '/tab/3/a' iv_append_tables = abap_true ).
    assign ref->* to <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = '' ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( mock-tab )
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
    data mock type ty_complex.

    prepare_cut(
      importing
        e_mock = mock " Must be here to keep reference alive
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
    data mock type ty_complex.
    lcl_json_to_abap=>bind(
      changing
        c_obj = mock
        co_instance = lo_cut ).

    data nodes type ref to lcl_nodes_helper.
    create object nodes.
    nodes->add( '/      |      |object |       | ' ).
    nodes->add( '/      |str   |str    |hello  | ' ).
    nodes->add( '/      |int   |num    |5      | ' ).
    nodes->add( '/      |float |num    |5.5    | ' ).
    nodes->add( '/      |bool  |bool   |true   | ' ).
    nodes->add( '/      |obj   |object |       | ' ).
    nodes->add( '/obj   |a     |str    |world  | ' ).
    nodes->add( '/      |tab   |array  |       | ' ).
    nodes->add( '/tab   |1     |object |       |1' ).
    nodes->add( '/tab/1 |a     |str    | One   | ' ).
    nodes->add( '/tab   |2     |object |       |2' ).
    nodes->add( '/tab/2 |a     |str    | Two   | ' ).

    lo_cut->to_abap( nodes->sorted( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = mock-str
      exp = 'hello' ).
    cl_abap_unit_assert=>assert_equals(
      act = mock-int
      exp = 5 ).
    cl_abap_unit_assert=>assert_equals(
      act = mock-float
      exp = '5.5' ).
    cl_abap_unit_assert=>assert_equals(
      act = mock-bool
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = mock-obj-a
      exp = 'world' ).

    data elem like line of mock-tab.
    cl_abap_unit_assert=>assert_equals(
      act = lines( mock-tab )
      exp = 2 ).

    read table mock-tab into elem index 1.
    cl_abap_unit_assert=>assert_equals(
      act = elem-a
      exp = 'One' ).
    read table mock-tab into elem index 2.
    cl_abap_unit_assert=>assert_equals(
      act = elem-a
      exp = 'Two' ).

  endmethod.

  method to_abap_negative.

    data lo_cut type ref to lcl_json_to_abap.
    data lx type ref to zcx_ajson_error.
    data mock type ty_complex.
    lcl_json_to_abap=>bind(
      changing
        c_obj = mock
        co_instance = lo_cut ).

    data nodes type ref to lcl_nodes_helper.

    try.
      create object nodes.
      nodes->add( '/    |      |object | ' ).
      nodes->add( '/    |str   |object | ' ).

      lo_cut->to_abap( nodes->sorted( ) ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Expected structure' ).
    endtry.

    try.
      create object nodes.
      nodes->add( '/    |      |object | ' ).
      nodes->add( '/    |str   |array  | ' ).

      lo_cut->to_abap( nodes->sorted( ) ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Expected table' ).
    endtry.

    try.
      create object nodes.
      nodes->add( '/    |      |object |      ' ).
      nodes->add( '/    |int   |str    |hello ' ).

      lo_cut->to_abap( nodes->sorted( ) ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Source is not a number' ).
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

    class-data gv_sample type string.

    methods set_ajson for testing raising zcx_ajson_error.
    methods set_value for testing raising zcx_ajson_error.
    methods set_obj for testing raising zcx_ajson_error.
    methods set_tab for testing raising zcx_ajson_error.
    methods prove_path_exists for testing raising zcx_ajson_error.
    methods delete_subtree for testing raising zcx_ajson_error.

endclass.

class zcl_ajson definition local friends ltcl_writer_test.

class ltcl_writer_test implementation.

  method prove_path_exists.

    data lo_cut type ref to zcl_ajson.
    data nodes_exp type ref to lcl_nodes_helper.

    lo_cut = zcl_ajson=>create_empty( ).

    create object nodes_exp.
    nodes_exp->add( '        |      |object |     ||1' ).
    nodes_exp->add( '/       |a     |object |     ||1' ).
    nodes_exp->add( '/a/     |b     |object |     ||1' ).
    nodes_exp->add( '/a/b/   |c     |object |     ||1' ).
    nodes_exp->add( '/a/b/c/ |d     |object |     ||0' ).

    lo_cut->prove_path_exists( '/a/b/c/d/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = nodes_exp->sorted( ) ).

    create object nodes_exp.
    nodes_exp->add( '         |      |object |     ||1' ).
    nodes_exp->add( '/        |a     |object |     ||1' ).
    nodes_exp->add( '/a/      |b     |object |     ||1' ).
    nodes_exp->add( '/a/b/    |c     |object |     ||1' ).
    nodes_exp->add( '/a/b/c/  |d     |object |     ||1' ).
    nodes_exp->add( '/a/b/c/d |e     |object |     ||0' ).
    lo_cut->prove_path_exists( '/a/b/c/d/e/' ).

  endmethod.

  method delete_subtree.

    data lo_cut type ref to zcl_ajson.
    data nodes_exp type ref to lcl_nodes_helper.

    lo_cut = zcl_ajson=>create_empty( ).

    create object nodes_exp.
    nodes_exp->add( '        |      |object |     ||1' ).
    nodes_exp->add( '/       |a     |object |     ||1' ).
    nodes_exp->add( '/a/     |b     |object |     ||1' ).
    nodes_exp->add( '/a/b/   |c     |object |     ||1' ).
    nodes_exp->add( '/a/b/c/ |d     |object |     ||0' ).

    lo_cut->mt_json_tree = nodes_exp->mt_nodes.

    create object nodes_exp.
    nodes_exp->add( '        |      |object |     ||1' ).
    nodes_exp->add( '/       |a     |object |     ||0' ).

    lo_cut->delete_subtree(
      iv_path = '/a/'
      iv_name = 'b' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = nodes_exp->sorted( ) ).

  endmethod.

  method set_ajson.

    data nodes type ref to lcl_nodes_helper.
    data lo_src type ref to zcl_ajson.
    data lo_cut type ref to zcl_ajson.
    data li_writer type ref to zif_ajson_writer.

    lo_src = zcl_ajson=>create_empty( ).
    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.

    " Prepare source
    create object nodes.
    nodes->add( '        |      |object |     ||1' ).
    nodes->add( '/       |x     |object |     ||2' ).
    nodes->add( '/x/     |b     |str    |abc  ||0' ).
    nodes->add( '/x/     |c     |num    |10   ||0' ).
    lo_src->mt_json_tree = nodes->mt_nodes.

    " Test 1 - assign root
    li_writer->set(
      iv_path = ''
      iv_val  = lo_src ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = nodes->sorted( ) ).

    li_writer->set(
      iv_path = '/'
      iv_val  = lo_src ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = nodes->sorted( ) ).

    " Test 2 - assign deep
    create object nodes.
    nodes->add( '        |      |object |     ||1' ).
    nodes->add( '/       |a     |object |     ||1' ).
    nodes->add( '/a/     |b     |object |     ||1' ).
    nodes->add( '/a/b/     |c     |object |     ||1' ).
    nodes->add( '/a/b/c/   |x     |object |     ||2' ).
    nodes->add( '/a/b/c/x/ |b     |str    |abc  ||0' ).
    nodes->add( '/a/b/c/x/ |c     |num    |10   ||0' ).

    li_writer->clear( ).
    li_writer->set(
      iv_path = '/a/b/c'
      iv_val  = lo_src ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = nodes->sorted( ) ).

    " Test 3 - assign rewrite
    create object nodes.
    nodes->add( '        |      |object |     ||1' ).
    nodes->add( '/       |a     |object |     ||1' ).
    nodes->add( '/a/       |b     |object |     ||1' ).
    nodes->add( '/a/b/     |x     |object |     ||2' ).
    nodes->add( '/a/b/x/   |b     |str    |abc  ||0' ).
    nodes->add( '/a/b/x/   |c     |num    |10   ||0' ).

    li_writer->set(
      iv_path = '/a/b'
      iv_val  = lo_src ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = nodes->sorted( ) ).

  endmethod.

  method set_value.

    data nodes type ref to lcl_nodes_helper.
    data lo_cut type ref to zcl_ajson.
    data li_writer type ref to zif_ajson_writer.

    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.

    " Prepare source
    create object nodes.
    nodes->add( '        |      |object |     ||1' ).
    nodes->add( '/       |x     |object |     ||2' ).
    nodes->add( '/x/     |b     |str    |abc  ||0' ).
    nodes->add( '/x/     |c     |num    |10   ||0' ).

    li_writer->set(
      iv_path = '/x/b'
      iv_val  = 'abc' ).
    li_writer->set(
      iv_path = '/x/c'
      iv_val  = 10 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = nodes->sorted( ) ).

  endmethod.

  method set_obj.

    data nodes type ref to lcl_nodes_helper.
    data lo_cut type ref to zcl_ajson.
    data li_writer type ref to zif_ajson_writer.

    data:
      begin of ls_struc,
        b type string value 'abc',
        c type i value 10,
      end of ls_struc.

    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.

    " Prepare source
    create object nodes.
    nodes->add( '        |      |object |     ||1' ).
    nodes->add( '/       |x     |object |     ||2' ).
    nodes->add( '/x/     |b     |str    |abc  ||0' ).
    nodes->add( '/x/     |c     |num    |10   ||0' ).

    li_writer->set(
      iv_path = '/x'
      iv_val  = ls_struc ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = nodes->sorted( ) ).

  endmethod.

  method set_tab.

    data nodes type ref to lcl_nodes_helper.
    data lo_cut type ref to zcl_ajson.
    data li_writer type ref to zif_ajson_writer.
    data lt_tab type string_table.

    lo_cut = zcl_ajson=>create_empty( ).
    li_writer = lo_cut.

    append 'hello' to lt_tab.
    append 'world' to lt_tab.

    " Prepare source
    create object nodes.
    nodes->add( '        |      |object |     ||1' ).
    nodes->add( '/       |x     |array  |     ||2' ).
    nodes->add( '/x/     |1     |str    |hello||0' ).
    nodes->add( '/x/     |2     |str    |world||0' ).

    li_writer->set(
      iv_path = '/x'
      iv_val  = lt_tab ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = nodes->sorted( ) ).

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

endclass.

class ltcl_integrated implementation.

  method array_simple.

    data lt_act type string_table.
    data lt_exp type string_table.
    data exp type string.

    data lv_src type string.
    lv_src = '['.
    do 10 times.
      if sy-index <> 1.
        lv_src = lv_src && `, `.
      endif.
      lv_src = lv_src && |"{ sy-index }"|.
      exp = |{ sy-index }|.
      append exp to lt_exp.
    enddo.
    lv_src = lv_src && ']'.

    data li_reader type ref to zif_ajson_reader.
    li_reader = zcl_ajson=>parse( lv_src ).
    li_reader->to_abap( importing ev_container = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  endmethod.

  method array_index.

    data lt_act type table of ty_loc.
    data lt_exp type table of ty_loc.
    data exp type ty_loc.

    data lv_src type string.
    lv_src = '['.
    do 10 times.
      if sy-index <> 1.
        lv_src = lv_src && `, `.
      endif.
      lv_src = lv_src && |\{ "row": { sy-index } \}|.
      exp-row = sy-index.
      append exp to lt_exp.
    enddo.
    lv_src = lv_src && ']'.

    data li_reader type ref to zif_ajson_reader.
    li_reader = zcl_ajson=>parse( lv_src ).
    li_reader->to_abap( importing ev_container = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  endmethod.

  method reader.

    data lv_source type string.
    data li_reader type ref to zif_ajson_reader.

    lv_source = ltcl_parser_test=>sample_json( ).
    li_reader = zcl_ajson=>parse( lv_source ).

    cl_abap_unit_assert=>assert_equals(
      act = li_reader->value( '/string' )
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
    methods set_obj for testing raising zcx_ajson_error.
    methods set_array for testing raising zcx_ajson_error.
    methods set_complex_obj for testing raising zcx_ajson_error.
    methods prefix for testing raising zcx_ajson_error.

endclass.

class zcl_ajson definition local friends ltcl_abap_to_json.

class ltcl_abap_to_json implementation.

  method set_ajson.

    data nodes type ref to lcl_nodes_helper.
    data lo_src type ref to zcl_ajson.
    lo_src = zcl_ajson=>create_empty( ).

    create object nodes.
    nodes->add( '        |      |object |     ||1' ).
    nodes->add( '/       |a     |object |     ||1' ).
    nodes->add( '/a/     |b     |object |     ||1' ).
    nodes->add( '/a/b/   |c     |object |     ||0' ).
    lo_src->mt_json_tree = nodes->mt_nodes.

    data lt_nodes type zcl_ajson=>ty_nodes_tt.
    lt_nodes = lcl_abap_to_json=>convert( iv_data = lo_src ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = nodes->mt_nodes ).

  endmethod.

  method set_value.

    data nodes_exp type ref to lcl_nodes_helper.
    data lt_nodes type zcl_ajson=>ty_nodes_tt.

    " number
    create object nodes_exp.
    nodes_exp->add( '        |      |num |1     ||' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = nodes_exp->mt_nodes ).

    " string
    create object nodes_exp.
    nodes_exp->add( '        |      |str |abc     ||' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = 'abc' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = nodes_exp->mt_nodes ).

    " true
    create object nodes_exp.
    nodes_exp->add( '        |      |bool |true     ||' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = nodes_exp->mt_nodes ).

    " false
    create object nodes_exp.
    nodes_exp->add( '        |      |bool |false    ||' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = nodes_exp->mt_nodes ).

    " xfeld
    data lv_xfeld type xfeld.
    create object nodes_exp.
    nodes_exp->add( '        |      |bool |true     ||' ).

    lv_xfeld = 'X'.
    lt_nodes = lcl_abap_to_json=>convert( iv_data = lv_xfeld ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = nodes_exp->mt_nodes ).

  endmethod.

  method prefix.

    data nodes_exp type ref to lcl_nodes_helper.
    data lt_nodes type zcl_ajson=>ty_nodes_tt.
    data ls_prefix type zcl_ajson=>ty_path_name.

    ls_prefix-path = '/a/'.
    ls_prefix-name = 'b'.
    create object nodes_exp.
    nodes_exp->add( '/a/       |b     |num |1     ||' ).

    lt_nodes = lcl_abap_to_json=>convert(
      iv_data   = 1
      is_prefix = ls_prefix ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = nodes_exp->mt_nodes ).

  endmethod.

  method set_obj.

    data nodes_exp type ref to lcl_nodes_helper.
    data ls_struc type ty_struc.
    data lt_nodes type zcl_ajson=>ty_nodes_tt.

    ls_struc-a = 'abc'.
    ls_struc-b = 10.
    ls_struc-c = abap_true.
    ls_struc-d = 'X'.

    create object nodes_exp.
    nodes_exp->add( '       |      |object |     ||4' ).
    nodes_exp->add( '/      |a     |str    |abc  ||0' ).
    nodes_exp->add( '/      |b     |num    |10   ||0' ).
    nodes_exp->add( '/      |c     |bool   |true ||0' ).
    nodes_exp->add( '/      |d     |bool   |true ||0' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = ls_struc ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = nodes_exp->mt_nodes ).

  endmethod.

  method set_complex_obj.

    data nodes_exp type ref to lcl_nodes_helper.
    data ls_struc type ty_struc_complex.
    data lt_nodes type zcl_ajson=>ty_nodes_tt.
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

    create object nodes_exp.
    nodes_exp->add( '       |      |object |     ||8' ).
    nodes_exp->add( '/      |a     |str    |abc  ||0' ).
    nodes_exp->add( '/      |b     |num    |10   ||0' ).
    nodes_exp->add( '/      |c     |bool   |true ||0' ).
    nodes_exp->add( '/      |d     |bool   |true ||0' ).
    nodes_exp->add( '/      |el    |str    |elem ||0' ).
    nodes_exp->add( '/      |struc |object |     ||4' ).
    nodes_exp->add( '/struc/|a     |str    |deep ||0' ).
    nodes_exp->add( '/struc/|b     |num    |123  ||0' ).
    nodes_exp->add( '/struc/|c     |bool   |false||0' ).
    nodes_exp->add( '/struc/|d     |bool   |false||0' ).

    nodes_exp->add( '/      |tab   |array  |     ||2' ).
    nodes_exp->add( '/tab/  |1     |object |     ||4' ).
    nodes_exp->add( '/tab/1/|a     |str    |abc  ||0' ).
    nodes_exp->add( '/tab/1/|b     |num    |0    ||0' ).
    nodes_exp->add( '/tab/1/|c     |bool   |false||0' ).
    nodes_exp->add( '/tab/1/|d     |bool   |false||0' ).
    nodes_exp->add( '/tab/  |2     |object |     ||4' ).
    nodes_exp->add( '/tab/2/|a     |str    |bcd  ||0' ).
    nodes_exp->add( '/tab/2/|b     |num    |0    ||0' ).
    nodes_exp->add( '/tab/2/|c     |bool   |false||0' ).
    nodes_exp->add( '/tab/2/|d     |bool   |false||0' ).

    nodes_exp->add( '/      |stab  |array  |     ||2' ).
    nodes_exp->add( '/stab/ |1     |str    |hello||0' ).
    nodes_exp->add( '/stab/ |2     |str    |world||0' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = ls_struc ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = nodes_exp->mt_nodes ).

  endmethod.

  method set_array.

    data nodes_exp type ref to lcl_nodes_helper.
    data lt_nodes type zcl_ajson=>ty_nodes_tt.

    data lt_tab type table of ty_struc.
    field-symbols <s> like line of lt_tab.

    append initial line to lt_tab assigning <s>.
    <s>-a = 'abc'.
    <s>-b = 10.
    append initial line to lt_tab assigning <s>.
    <s>-a = 'bcd'.
    <s>-b = 20.

    create object nodes_exp.
    nodes_exp->add( '       |      |array  |     ||2' ).
    nodes_exp->add( '/      |1     |object |     ||4' ).
    nodes_exp->add( '/1/    |a     |str    |abc  ||0' ).
    nodes_exp->add( '/1/    |b     |num    |10   ||0' ).
    nodes_exp->add( '/1/    |c     |bool   |false||0' ).
    nodes_exp->add( '/1/    |d     |bool   |false||0' ).
    nodes_exp->add( '/      |2     |object |     ||4' ).
    nodes_exp->add( '/2/    |a     |str    |bcd  ||0' ).
    nodes_exp->add( '/2/    |b     |num    |20   ||0' ).
    nodes_exp->add( '/2/    |c     |bool   |false||0' ).
    nodes_exp->add( '/2/    |d     |bool   |false||0' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = lt_tab ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = nodes_exp->mt_nodes ).

    data lt_strtab type string_table.
    append 'abc' to lt_strtab.
    append 'bcd' to lt_strtab.

    create object nodes_exp.
    nodes_exp->add( '       |      |array  |     ||2' ).
    nodes_exp->add( '/      |1     |str    |abc  ||0' ).
    nodes_exp->add( '/      |2     |str    |bcd  ||0' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = lt_strtab ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = nodes_exp->mt_nodes ).

  endmethod.

endclass.
