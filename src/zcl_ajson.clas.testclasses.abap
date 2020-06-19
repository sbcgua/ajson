
**********************************************************************
* PARSER
**********************************************************************

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

**********************************************************************
* JSON TO ABAP
**********************************************************************

class ltcl_json_to_abap definition
  for testing
  risk level harmless
  duration short
  final.

  public section.

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

    data mt_nodes type zcl_ajson=>ty_nodes_tt.
    methods _node
      importing
        iv_str type string.

endclass.

class ltcl_json_to_abap implementation.

  method _node.

    field-symbols <n> like line of mt_nodes.
    data lv_children type string.

    append initial line to mt_nodes assigning <n>.

    split iv_str at '|' into
      <n>-path
      <n>-name
      <n>-type
      <n>-value
      lv_children.
    condense <n>-path.
    condense <n>-name.
    condense <n>-type.
    condense <n>-value.
    <n>-children = lv_children.

  endmethod.

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

    eo_cut = lcl_json_to_abap=>bind( changing c_obj = e_mock ).

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
    lo_cut = lcl_json_to_abap=>bind( changing c_obj = mock ).

    clear mt_nodes.
    _node( '/    |      |object |       ' ).
    _node( '/    |str   |str    |hello  ' ).
    _node( '/    |int   |num    |5      ' ).
    _node( '/    |float |num    |5.5    ' ).
    _node( '/    |bool  |bool   |true   ' ).
    _node( '/    |obj   |object |       ' ).
    _node( '/obj |a     |str    |world  ' ).
    _node( '/    |tab   |array  |       ' ).
    _node( '/tab   |1     |object |       ' ).
    _node( '/tab/1 |a     |str    | One   ' ).
    _node( '/tab   |2     |object |       ' ).
    _node( '/tab/2 |a     |str    | Two   ' ).

    lo_cut->to_abap( mt_nodes ).

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
    lo_cut = lcl_json_to_abap=>bind( changing c_obj = mock ).


    try.
      clear mt_nodes.
      _node( '/    |      |object | ' ).
      _node( '/    |str   |object | ' ).

      lo_cut->to_abap( mt_nodes ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Expected structure' ).
    endtry.

    try.
      clear mt_nodes.
      _node( '/    |      |object | ' ).
      _node( '/    |str   |array  | ' ).

      lo_cut->to_abap( mt_nodes ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Expected table' ).
    endtry.

    try.
      clear mt_nodes.
      _node( '/    |      |object |      ' ).
      _node( '/    |int   |str    |hello ' ).

      lo_cut->to_abap( mt_nodes ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Source is not a number' ).
    endtry.


  endmethod.

endclass.
