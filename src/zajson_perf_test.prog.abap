*&---------------------------------------------------------------------*
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
    methods run raising cx_static_check.
    methods print.

  private section.
    data mo_object type ref to object.
    data mv_method type string.
    data mv_times type i.
    data mv_diff type p decimals 6.
endclass.

class lcl_benchmark implementation.

  method constructor.
    mo_object = io_object.
    mv_method = to_upper( iv_method ).
    mv_times = iv_times.
  endmethod.

  method run.
    data:
      lv_sta_time     type timestampl,
      lv_end_time     type timestampl.

    get time stamp field lv_sta_time.
    do mv_times times.
      call method mo_object->(mv_method).
    enddo.
    get time stamp field lv_end_time.
    mv_diff  = lv_end_time - lv_sta_time.

  endmethod.

  method print.
    write: /(30) mv_method, 'results', mv_diff  exponent 0.
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
      raising
        cx_static_check.

    data mv_num_rounds type i.

endclass.

class lcl_runner_base implementation.

  method run.

    data lo_benchmark type ref to lcl_benchmark.

    create object lo_benchmark
      exporting
        io_object = me
        iv_method = iv_method
        iv_times  = mv_num_rounds.

    lo_benchmark->run( ).
    lo_benchmark->print( ).

  endmethod.

endclass.

**********************************************************************
* END OF CONTRIB from https://github.com/sbcgua/benchmarks
**********************************************************************

class lcl_app definition final.
  public section.

    methods plain_obj raising cx_static_check.
    methods deep_obj raising cx_static_check.
    methods array raising cx_static_check.

    class-methods main.
    methods prepare.
    methods run
      importing
        iv_method type string
      raising
        cx_static_check.

    data mv_num_rounds type i.

    data mv_plain_obj type string.
    data mv_deep_obj type string.
    data mv_array type string.

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

  method run.

    data lo_benchmark type ref to lcl_benchmark.

    create object lo_benchmark
      exporting
        io_object = me
        iv_method = iv_method
        iv_times  = mv_num_rounds.

    lo_benchmark->run( ).
    lo_benchmark->print( ).

  endmethod.

  method main.

    data lo_app type ref to lcl_app.
    data lx type ref to cx_root.

    create object lo_app.

    lo_app->prepare( ).
    lo_app->mv_num_rounds = 1000.

    try.
      lo_app->run( 'plain_obj' ).
      lo_app->run( 'deep_obj' ).
      lo_app->run( 'array' ).
    catch cx_root into lx.
      write: / lx->get_text( ).
    endtry.

  endmethod.

endclass.

start-of-selection.

  lcl_app=>main( ).
