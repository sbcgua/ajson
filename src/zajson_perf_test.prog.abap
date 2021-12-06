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

    methods plain_obj raising cx_static_check.
    methods deep_obj raising cx_static_check.
    methods array raising cx_static_check.
    methods big_and_complex raising cx_static_check.

    class-methods main.
    methods prepare.
    methods prepare_complex.

    data mv_plain_obj type string.
    data mv_deep_obj type string.
    data mv_array type string.
    data mv_big_and_complex type string.
    data mo_complex_type type ref to cl_abap_structdescr.
    data mr_complex_data type ref to data.

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
    mv_plain_obj =
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

    mv_deep_obj =
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

    mv_array = '['.
    do 10 times.
      if sy-index <> 1.
        mv_array = mv_array && `, `.
      endif.
      mv_array = mv_array &&
        '{' &&
        '    "string": "abc", ' &&
        '    "number": 123,   ' &&
        '    "float": 123.45  ' &&
        '}'.
    enddo.
    mv_array = mv_array && ']'.

    prepare_complex( ).

  endmethod.

  method prepare_complex.

    constants lc_fields  type i value 256.
    constants lc_tabrows type i value 256.

    data lo_long_struc type ref to cl_abap_structdescr.
    data lo_long_table type ref to cl_abap_tabledescr.
    data lo_field_type type ref to cl_abap_datadescr.
    data lt_components type cl_abap_structdescr=>component_table.
    data ls_comp like line of lt_components.
    data lv_data type i.

    lo_field_type ?= cl_abap_typedescr=>describe_by_name( 'CHAR10' ).
    ls_comp-type = lo_field_type.
    do lc_fields times.
      ls_comp-name = |C{ sy-index }|.
      append ls_comp to lt_components.
    enddo.

    lo_long_struc = cl_abap_structdescr=>create( lt_components ).
    lo_long_table = cl_abap_tabledescr=>create( lo_long_struc ).
    ls_comp-type ?= lo_long_table.
    ls_comp-name  = 'TAB'.
    append ls_comp to lt_components.

    mo_complex_type = cl_abap_structdescr=>create( lt_components ).
    create data mr_complex_data type handle mo_complex_type.

    " Data
    mv_big_and_complex = '{'.
    do lc_fields times.
      if sy-index <> 1.
        mv_big_and_complex = mv_big_and_complex && `, `.
      endif.
      lv_data = lv_data + 1.
      mv_big_and_complex = mv_big_and_complex && |"C{ sy-index }": "{ lv_data }"|.
    enddo.

    mv_big_and_complex = mv_big_and_complex && ', "TAB": ['.

    do lc_tabrows times.
      if sy-index <> 1.
        mv_big_and_complex = mv_big_and_complex && `, `.
      endif.
      mv_big_and_complex = mv_big_and_complex && '{'.
      do lc_fields times.
        if sy-index <> 1.
          mv_big_and_complex = mv_big_and_complex && `, `.
        endif.
        lv_data = lv_data + 1.
        mv_big_and_complex = mv_big_and_complex && |"C{ sy-index }": "{ lv_data }"|.
      enddo.
      mv_big_and_complex = mv_big_and_complex && '}'.
    enddo.

    mv_big_and_complex = mv_big_and_complex && ']}'.

  endmethod.

  method plain_obj.

    data lo_json type ref to zif_ajson.
    data ls_target type ty_plain.
    lo_json = zcl_ajson=>parse( mv_plain_obj ).
    lo_json->to_abap( importing ev_container = ls_target ).

  endmethod.

  method deep_obj.

    data lo_json type ref to zif_ajson.
    data ls_target type ty_deep3.
    lo_json = zcl_ajson=>parse( mv_deep_obj ).
    lo_json->to_abap( importing ev_container = ls_target ).

  endmethod.

  method array.

    data lo_json type ref to zif_ajson.
    data ls_target type tt_fragment.
    lo_json = zcl_ajson=>parse( mv_array ).
    lo_json->to_abap( importing ev_container = ls_target ).

  endmethod.

  method big_and_complex.

    data lo_json type ref to zif_ajson.
    field-symbols <data> type any.
    assign mr_complex_data->* to <data>.
    lo_json = zcl_ajson=>parse( mv_big_and_complex ).
    lo_json->to_abap( importing ev_container = <data> ).

  endmethod.

  method main.

    data lo_app type ref to lcl_app.
    data lx type ref to cx_root.
    data lv_date type string.

    create object lo_app.

    lo_app->prepare( ).
    lo_app->mv_num_rounds = 1000.

    lv_date = |{ sy-datum+0(4) }-{ sy-datum+4(2) }-{ sy-datum+6(2) }|.
    write: / 'Date', lv_date.

    try.
      lo_app->run( 'plain_obj' ).
      lo_app->run( 'deep_obj' ).
      lo_app->run( 'array' ).
      lo_app->run( iv_method = 'big_and_complex' iv_times = 10 ).
    catch cx_root into lx.
      write: / lx->get_text( ).
    endtry.

  endmethod.

endclass.

start-of-selection.

  lcl_app=>main( ).
