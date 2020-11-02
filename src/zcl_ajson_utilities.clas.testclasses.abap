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
      importing
        iv_separator   type string optional
      returning
        value(rv_json) type string.

endclass.

class ltcl_parser_test implementation.

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

endclass.

**********************************************************************
* JSON DIFF
**********************************************************************

class ltcl_json_diff definition
  for testing
  risk level harmless
  duration short
  final.

  private section.

    methods diff for testing raising zcx_ajson_error.

endclass.

class zcl_ajson_utilities definition local friends ltcl_json_diff.

class ltcl_json_diff implementation.

  method diff.

    data:
      lv_json       type string,
      lo_diff       type ref to zcl_ajson_utilities,
      lo_insert     type ref to zcl_ajson,
      lo_delete     type ref to zcl_ajson,
      lo_change     type ref to zcl_ajson,
      lo_insert_exp type ref to lcl_nodes_helper,
      lo_delete_exp type ref to lcl_nodes_helper,
      lo_change_exp type ref to lcl_nodes_helper.

    lv_json =
      '{\n' &&
      '  "string": "abc",\n' && "no changes
      '  "number": 789,\n' &&   "changed value
      '  "float": 123.45,\n' &&
      '  "boolean": "true",\n' && " changed type
      '  "true": true,\n' &&    " insert
*      '  "false": false,\n' &&    " delete
      '  "null": null,\n' &&
      '  "date": "2020-03-15",\n' &&
      '  "issues": [\n' &&
      '    {\n' &&
      '      "message": "Indentation problem ...",\n' &&
      '      "key": "indentation",\n' &&
      '      "start": {\n' &&
      '        "row": 5,\n' &&  " array change
      '        "col": 3\n' &&
      '      },\n' &&
      '      "end": {\n' &&
      '        "new": 1,\n' &&  " array insert
*      '        "row": 4,\n' && " array delete
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

    replace all occurrences of '\n' in lv_json with cl_abap_char_utilities=>newline.

    create object lo_insert_exp.
    lo_insert_exp->add( '                |        |object |        |0|2' ).
    lo_insert_exp->add( '/               |issues  |array  |        |0|1' ).
    lo_insert_exp->add( '/               |true    |bool   |true    |0|0' ).
    lo_insert_exp->add( '/issues/        |1       |object |        |1|1' ).
    lo_insert_exp->add( '/issues/1/      |end     |object |        |0|1' ).
    lo_insert_exp->add( '/issues/1/end/  |new     |num    |1       |0|0' ).

    create object lo_delete_exp.
    lo_delete_exp->add( '                |        |object |        |0|2' ).
    lo_delete_exp->add( '/               |false   |bool   |false   |0|0' ).
    lo_delete_exp->add( '/               |issues  |array  |        |0|1' ).
    lo_delete_exp->add( '/issues/        |1       |object |        |1|1' ).
    lo_delete_exp->add( '/issues/1/      |end     |object |        |0|1' ).
    lo_delete_exp->add( '/issues/1/end/  |row     |num    |4       |0|0' ).

    create object lo_change_exp.
    lo_change_exp->add( '                |        |object |        |0|3' ).
    lo_change_exp->add( '/               |boolean |str    |true    |0|0' ).
    lo_change_exp->add( '/               |issues  |array  |        |0|1' ).
    lo_change_exp->add( '/               |number  |num    |789     |0|0' ).
    lo_change_exp->add( '/issues/        |1       |object |        |1|1' ).
    lo_change_exp->add( '/issues/1/      |start   |object |        |0|1' ).
    lo_change_exp->add( '/issues/1/start/|row     |num    |5       |0|0' ).

    create object lo_diff.

    lo_diff->diff(
      exporting
        iv_json_a = ltcl_parser_test=>sample_json( )
        iv_json_b = lv_json
      importing
        eo_insert = lo_insert
        eo_delete = lo_delete
        eo_change = lo_change ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_insert->mt_json_tree
      exp = lo_insert_exp->mt_nodes ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_delete->mt_json_tree
      exp = lo_delete_exp->mt_nodes ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_change->mt_json_tree
      exp = lo_change_exp->mt_nodes ).

  endmethod.

endclass.
