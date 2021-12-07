report zajson_perf_test.

**********************************************************************
* CONTRIB from https://github.com/sbcgua/benchmarks
**********************************************************************

class lcl_benchmark definition final.
  public section.
    methods constructor
      importing
        io_object type ref to object
        iv_method type string
        iv_times  type i.
    methods run
      raising
        cx_static_check.
    methods print.

  private section.
    data mo_object type ref to object.
    data mv_method type string.
    data mv_times type i.
    data mv_diff type tzntstmpl.
endclass.

class lcl_benchmark implementation.

  method constructor.
    mo_object = io_object.
    mv_method = to_upper( iv_method ).
    mv_times = iv_times.
  endmethod.

  method run.
    data lv_sta_time type timestampl.
    data lv_end_time type timestampl.

    get time stamp field lv_sta_time.
    do mv_times times.
      call method mo_object->(mv_method).
    enddo.
    get time stamp field lv_end_time.

    mv_diff  = cl_abap_tstmp=>subtract(
      tstmp1 = lv_end_time
      tstmp2 = lv_sta_time ).

  endmethod.

  method print.
    data lv_rounds type string.
    data lv_result type string.
    lv_rounds = |rounds: { mv_times }|.
    lv_result = |result: { mv_diff }|.
    write: /(30) mv_method, (20) lv_rounds, lv_result.
    uline.
  endmethod.

endclass.

**********************************************************************
* RUNNER
**********************************************************************

class lcl_runner_base definition.
  public section.

    methods run
      importing
        iv_method type string
        iv_times type i optional
      raising
        cx_static_check.

  protected section.
    data mv_num_rounds type i.

endclass.

class lcl_runner_base implementation.

  method run.

    data lo_benchmark type ref to lcl_benchmark.
    data lv_times type i.

    if iv_times > 0.
      lv_times = iv_times.
    else.
      lv_times = mv_num_rounds.
    endif.

    create object lo_benchmark
      exporting
        io_object = me
        iv_method = iv_method
        iv_times  = lv_times.

    lo_benchmark->run( ).
    lo_benchmark->print( ).

  endmethod.

endclass.

**********************************************************************
* END OF CONTRIB from https://github.com/sbcgua/benchmarks
**********************************************************************

class lcl_app definition final inheriting from lcl_runner_base.
  public section.

    methods parse_plain_obj raising cx_static_check.
    methods parse_deep_obj raising cx_static_check.
    methods parse_array raising cx_static_check.
    methods parse_long_array raising cx_static_check.
    methods parse_complex raising cx_static_check.

    methods to_abap_plain_obj raising cx_static_check.
    methods to_abap_deep_obj raising cx_static_check.
    methods to_abap_array raising cx_static_check.
    methods to_abap_long_array raising cx_static_check.
    methods to_abap_complex raising cx_static_check.

    class-methods main.
    methods prepare.
    methods prepare_parsed raising cx_static_check.
    methods prepare_complex.

    methods prepare_components
      importing
        iv_fields type i
      returning
        value(rt_components) type cl_abap_structdescr=>component_table.
    methods prepare_json_object
      importing
        iv_fields     type i
        iv_start_data type i default 0
      returning
        value(rv_str) type string.

    methods prepare_long_array.
    methods prepare_long_array_container
      importing
        iv_fields type i.
    methods prepare_long_array_str
      importing
        iv_fields type i
        iv_lines type i.

    data mv_json_plain_obj type string.
    data mv_json_deep type string.
    data mv_json_array type string.
    data mv_json_long_array type string.
    data mv_json_complex type string.

    data mr_complex_data type ref to data.
    data mr_long_array type ref to data.

    data mo_plain_obj type ref to zif_ajson.
    data mo_deep type ref to zif_ajson.
    data mo_array type ref to zif_ajson.
    data mo_long_array type ref to zif_ajson.
    data mo_complex type ref to zif_ajson.

    types:
      begin of ty_fragment,
        string type string,
        number type i,
        float type f,
      end of ty_fragment,
      tt_fragment type standard table of ty_fragment with default key,
      begin of ty_plain.
        include type ty_fragment.
        types:
        boolean type abap_bool,
        false type abap_bool,
        null type string,
        date type string, " ??? TODO
        str1 type string,
        str2 type string,
      end of ty_plain,
      begin of ty_deep1.
        include type ty_fragment.
        types: deep type ty_fragment,
      end of ty_deep1,
      begin of ty_deep2.
        include type ty_fragment.
        types: deep type ty_deep1,
      end of ty_deep2,
      begin of ty_deep3.
        include type ty_fragment.
        types: deep type ty_deep2,
      end of ty_deep3.

endclass.

class lcl_app implementation.

  method prepare.
    mv_json_plain_obj =
      '{' &&
      '  "string": "abc",' &&
      '  "number": 123,' &&
      '  "float": 123.45,' &&
      '  "boolean": true,' &&
      '  "false": false,' &&
      '  "null": null,' &&
      '  "date": "2020-03-15",' &&
      '  "str1": "hello",' &&
      '  "str2": "world"' &&
      '}'.

    mv_json_deep =
      '{' &&
      '    "string": "abc",' &&
      '    "number": 123,' &&
      '    "float": 123.45,' &&
      '    "deep" : {' &&
      '        "string": "abc",' &&
      '        "number": 223,' &&
      '        "float": 123.45,' &&
      '        "deep" : {' &&
      '            "string": "abc",' &&
      '            "number": 323,' &&
      '            "float": 123.45,' &&
      '            "deep" : {' &&
      '                "string": "abc",' &&
      '                "number": 423,  ' &&
      '                "float": 123.45 ' &&
      '            }' &&
      '        }' &&
      '    }' &&
      '}'.

    mv_json_array = '['.
    do 10 times.
      if sy-index <> 1.
        mv_json_array = mv_json_array && `, `.
      endif.
      mv_json_array = mv_json_array &&
        '{' &&
        '    "string": "abc", ' &&
        '    "number": 123,   ' &&
        '    "float": 123.45  ' &&
        '}'.
    enddo.
    mv_json_array = mv_json_array && ']'.

    prepare_complex( ).
    prepare_long_array( ).

  endmethod.

  method prepare_complex.

    constants lc_fields  type i value 256.
    constants lc_tabrows type i value 256.

    data lo_long_struc type ref to cl_abap_structdescr.
    data lo_long_table type ref to cl_abap_tabledescr.
    data lt_components type cl_abap_structdescr=>component_table.
    data lv_data type i.
    data lo_complex_type type ref to cl_abap_structdescr.
    data ls_comp like line of lt_components.

    lt_components = prepare_components( lc_fields ).

    lo_long_struc = cl_abap_structdescr=>create( lt_components ).
    lo_long_table = cl_abap_tabledescr=>create( lo_long_struc ).
    ls_comp-type ?= lo_long_table.
    ls_comp-name  = 'TAB'.
    append ls_comp to lt_components.

    lo_complex_type = cl_abap_structdescr=>create( lt_components ).
    create data mr_complex_data type handle lo_complex_type.

    " Data
    mv_json_complex = prepare_json_object(
        iv_fields     = lc_fields
        iv_start_data = lv_data ).
    lv_data = lv_data + lc_fields.

    mv_json_complex = replace( val = mv_json_complex sub = '}' with = `, "TAB": [` ).

    data lt_tab type string_table.

    do lc_tabrows times.
      append prepare_json_object(
        iv_fields = lc_fields
        iv_start_data = lv_data ) to lt_tab.
      lv_data = lv_data + lc_fields.
    enddo.

    mv_json_complex = mv_json_complex && concat_lines_of( table = lt_tab sep = `, ` ) && `]}`.

  endmethod.

  method prepare_long_array.

    constants lc_fields  type i value 20.
    constants lc_tabrows type i value 5000.

    prepare_long_array_container( iv_fields = lc_fields ).
    prepare_long_array_str(
      iv_fields = lc_fields
      iv_lines  = lc_tabrows ).

  endmethod.

  method prepare_components.

    data lo_field_type type ref to cl_abap_datadescr.
    data ls_comp like line of rt_components.

    lo_field_type ?= cl_abap_typedescr=>describe_by_name( 'CHAR10' ).
    ls_comp-type = lo_field_type.
    do iv_fields times.
      ls_comp-name = |C{ sy-index }|.
      append ls_comp to rt_components.
    enddo.

  endmethod.

  method prepare_long_array_container.

    data lo_long_struc type ref to cl_abap_structdescr.
    data lo_long_table type ref to cl_abap_tabledescr.
    data lt_components type cl_abap_structdescr=>component_table.

    lt_components = prepare_components( iv_fields ).
    lo_long_struc = cl_abap_structdescr=>create( lt_components ).
    lo_long_table = cl_abap_tabledescr=>create( lo_long_struc ).

    create data mr_long_array type handle lo_long_table.

  endmethod.

  method prepare_json_object.

    data lv_data type i.
    data lt_tab type string_table.
    data lv_tmp type string.

    lv_data = iv_start_data.

    do iv_fields times.
      lv_data = lv_data + 1.
      lv_tmp = |"C{ sy-index }": "{ lv_data }"|.
      append lv_tmp to lt_tab.
    enddo.

    rv_str = `{` && concat_lines_of( table = lt_tab sep = `, ` ) && `}`.

  endmethod.

  method prepare_long_array_str.

    data lt_tab type string_table.
    data lv_data type i.

    do iv_lines times.
      append prepare_json_object(
        iv_fields = iv_fields
        iv_start_data = lv_data ) to lt_tab.
      lv_data = lv_data + iv_fields.
    enddo.

    mv_json_long_array = `[` && concat_lines_of( table = lt_tab sep = `, ` ) && `]`.

  endmethod.

  method prepare_parsed.

    mo_plain_obj  = zcl_ajson=>parse( mv_json_plain_obj ).
    mo_deep       = zcl_ajson=>parse( mv_json_deep ).
    mo_array      = zcl_ajson=>parse( mv_json_array ).
    mo_complex    = zcl_ajson=>parse( mv_json_complex ).
    mo_long_array = zcl_ajson=>parse( mv_json_long_array ).

  endmethod.

  method parse_plain_obj.

    data lo_json type ref to zif_ajson.
    lo_json = zcl_ajson=>parse( mv_json_plain_obj ).

  endmethod.

  method parse_deep_obj.

    data lo_json type ref to zif_ajson.
    lo_json = zcl_ajson=>parse( mv_json_deep ).

  endmethod.

  method parse_array.

    data lo_json type ref to zif_ajson.
    lo_json = zcl_ajson=>parse( mv_json_array ).

  endmethod.

  method parse_long_array.

    data lo_json type ref to zif_ajson.
    lo_json = zcl_ajson=>parse( mv_json_long_array ).

  endmethod.

  method parse_complex.

    data lo_json type ref to zif_ajson.
    lo_json = zcl_ajson=>parse( mv_json_complex ).

  endmethod.

  method to_abap_plain_obj.

    data ls_target type ty_plain.
    mo_plain_obj->to_abap( importing ev_container = ls_target ).

  endmethod.

  method to_abap_deep_obj.

    data ls_target type ty_deep3.
    mo_deep->to_abap( importing ev_container = ls_target ).

  endmethod.

  method to_abap_array.

    data ls_target type tt_fragment.
    mo_array->to_abap( importing ev_container = ls_target ).

  endmethod.

  method to_abap_long_array.

    field-symbols <data> type any.
    assign mr_long_array->* to <data>.
    mo_long_array->to_abap( importing ev_container = <data> ).

  endmethod.

  method to_abap_complex.

    field-symbols <data> type any.
    assign mr_complex_data->* to <data>.
    mo_complex->to_abap( importing ev_container = <data> ).

  endmethod.

  method main.

    data lo_app type ref to lcl_app.
    data lx type ref to cx_root.
    data lv_tmp type string.

    create object lo_app.

    lo_app->mv_num_rounds = 1000.

    lv_tmp = |{ sy-datum+0(4) }-{ sy-datum+4(2) }-{ sy-datum+6(2) }|.
    write: / 'Date', lv_tmp.

    try.

      lo_app->prepare( ).

      lo_app->run( 'parse_plain_obj' ).
      lo_app->run( 'parse_deep_obj' ).
      lo_app->run( 'parse_array' ).
      lo_app->run(
        iv_method = 'parse_long_array'
        iv_times  = 5 ).
      lo_app->run(
        iv_method = 'parse_complex'
        iv_times  = 5 ).

      lo_app->prepare_parsed( ).

      lo_app->run( 'to_abap_plain_obj' ).
      lo_app->run( 'to_abap_deep_obj' ).
      lo_app->run( 'to_abap_array' ).
      lo_app->run(
        iv_method = 'to_abap_long_array'
        iv_times  = 5 ).
      lo_app->run(
        iv_method = 'to_abap_complex'
        iv_times  = 5 ).

    catch cx_root into lx.
      lv_tmp = lx->get_text( ).
      write: / 'Exception raised:', lv_tmp.
    endtry.

  endmethod.

endclass.

start-of-selection.

  lcl_app=>main( ).
