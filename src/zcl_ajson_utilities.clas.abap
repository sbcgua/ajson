class zcl_ajson_utilities definition
  public
  create public .

  public section.

    methods diff
      importing
        !iv_json_a            type string optional
        !iv_json_b            type string optional
        !io_json_a            type ref to zif_ajson optional
        !io_json_b            type ref to zif_ajson optional
        !iv_keep_empty_arrays type abap_bool default abap_false
      exporting
        !eo_insert            type ref to zif_ajson
        !eo_delete            type ref to zif_ajson
        !eo_change            type ref to zif_ajson
      raising
        zcx_ajson_error .
    methods sort
      importing
        !iv_json         type string optional
        !io_json         type ref to zif_ajson optional
      returning
        value(rv_sorted) type string
      raising
        zcx_ajson_error .
  protected section.

  private section.

    data mo_json_a type ref to zif_ajson .
    data mo_json_b type ref to zif_ajson .
    data mo_insert type ref to zif_ajson .
    data mo_delete type ref to zif_ajson .
    data mo_change type ref to zif_ajson .

    methods check_input
      importing
        !iv_json       type string optional
        !io_json       type ref to zif_ajson optional
      returning
        value(ro_json) type ref to zif_ajson
      raising
        zcx_ajson_error .
    methods diff_a_b
      importing
        !iv_path type string
      raising
        zcx_ajson_error .
    methods diff_b_a
      importing
        !iv_path type string
      raising
        zcx_ajson_error .
    methods delete_empty_nodes
      importing
        !io_json              type ref to zif_ajson
        !iv_keep_empty_arrays type abap_bool
      raising
        zcx_ajson_error .
ENDCLASS.



CLASS zcl_ajson_utilities IMPLEMENTATION.


  method check_input.

    if boolc( iv_json is initial ) = boolc( io_json is initial ).
      zcx_ajson_error=>raise( 'Either supply JSON string or instance, but not both' ).
    endif.

    if iv_json is not initial.
      ro_json = zcl_ajson=>parse( iv_json ).
    elseif io_json is not initial.
      ro_json = io_json.
    else.
      zcx_ajson_error=>raise( 'Supply either JSON string or instance' ).
    endif.

  endmethod.


  method delete_empty_nodes.

    data ls_json_tree like line of io_json->mt_json_tree.
    data lv_done type abap_bool.

    do.
      lv_done = abap_true.

      if iv_keep_empty_arrays = abap_false.
        loop at io_json->mt_json_tree into ls_json_tree
          where type = 'array' and children = 0.

          io_json->delete( ls_json_tree-path && ls_json_tree-name ).

        endloop.
        if sy-subrc = 0.
          lv_done = abap_false.
        endif.
      endif.

      loop at io_json->mt_json_tree into ls_json_tree
        where type = 'object' and children = 0.

        io_json->delete( ls_json_tree-path && ls_json_tree-name ).

      endloop.
      if sy-subrc = 0.
        lv_done = abap_false.
      endif.

      if lv_done = abap_true.
        exit. " nothing else to delete
      endif.
    enddo.

  endmethod.


  method diff.

    mo_json_a = check_input(
      iv_json = iv_json_a
      io_json = io_json_a ).

    mo_json_b = check_input(
      iv_json = iv_json_b
      io_json = io_json_b ).

    mo_insert = zcl_ajson=>create_empty( ).
    mo_delete = zcl_ajson=>create_empty( ).
    mo_change = zcl_ajson=>create_empty( ).

    diff_a_b( '/' ).
    diff_b_a( '/' ).

    eo_insert ?= mo_insert.
    eo_delete ?= mo_delete.
    eo_change ?= mo_change.

    delete_empty_nodes(
      io_json              = eo_insert
      iv_keep_empty_arrays = iv_keep_empty_arrays ).
    delete_empty_nodes(
      io_json              = eo_delete
      iv_keep_empty_arrays = iv_keep_empty_arrays ).
    delete_empty_nodes(
      io_json              = eo_change
      iv_keep_empty_arrays = iv_keep_empty_arrays ).

  endmethod.


  method diff_a_b.

    data:
      lv_path_a type string,
      lv_path_b type string.

    field-symbols:
      <node_a> like line of mo_json_a->mt_json_tree,
      <node_b> like line of mo_json_a->mt_json_tree.

    loop at mo_json_a->mt_json_tree assigning <node_a> where path = iv_path.
      lv_path_a = <node_a>-path && <node_a>-name && '/'.

      read table mo_json_b->mt_json_tree assigning <node_b>
        with table key path = <node_a>-path name = <node_a>-name.
      if sy-subrc = 0.
        lv_path_b = <node_b>-path && <node_b>-name && '/'.

        if <node_a>-type = <node_b>-type.
          case <node_a>-type.
            when 'array'.
              mo_insert->touch_array( lv_path_a ).
              mo_change->touch_array( lv_path_a ).
              mo_delete->touch_array( lv_path_a ).
              diff_a_b( lv_path_a ).
            when 'object'.
              diff_a_b( lv_path_a ).
            when others.
              if <node_a>-value <> <node_b>-value.
                " save as changed value
                mo_change->set(
                  iv_path      = lv_path_b
                  iv_val       = <node_b>-value
                  iv_node_type = <node_b>-type ).
              endif.
          endcase.
        else.
          " save changed type as delete + insert
          case <node_a>-type.
            when 'array'.
              mo_delete->touch_array( lv_path_a ).
              diff_a_b( lv_path_a ).
            when 'object'.
              diff_a_b( lv_path_a ).
            when others.
              mo_delete->set(
                iv_path      = lv_path_a
                iv_val       = <node_a>-value
                iv_node_type = <node_a>-type ).
          endcase.
          case <node_b>-type.
            when 'array'.
              mo_insert->touch_array( lv_path_b ).
              diff_b_a( lv_path_b ).
            when 'object'.
              diff_b_a( lv_path_b ).
            when others.
              mo_insert->set(
                iv_path      = lv_path_b
                iv_val       = <node_b>-value
                iv_node_type = <node_b>-type ).
          endcase.
        endif.
      else.
        " save as delete
        case <node_a>-type.
          when 'array'.
            mo_delete->touch_array( lv_path_a ).
            diff_a_b( lv_path_a ).
          when 'object'.
            diff_a_b( lv_path_a ).
          when others.
            mo_delete->set(
              iv_path      = lv_path_a
              iv_val       = <node_a>-value
              iv_node_type = <node_a>-type ).
        endcase.
      endif.
    endloop.

  endmethod.


  method diff_b_a.

    data lv_path type string.

    field-symbols:
      <node_a> like line of mo_json_b->mt_json_tree,
      <node_b> like line of mo_json_b->mt_json_tree.

    loop at mo_json_b->mt_json_tree assigning <node_b> where path = iv_path.
      lv_path = <node_b>-path && <node_b>-name && '/'.

      case <node_b>-type.
        when 'array'.
          mo_insert->touch_array( lv_path ).
          diff_b_a( lv_path ).
        when 'object'.
          diff_b_a( lv_path ).
        when others.
          read table mo_json_a->mt_json_tree assigning <node_a>
            with table key path = <node_b>-path name = <node_b>-name.
          if sy-subrc <> 0.
            " save as insert
            mo_insert->set(
              iv_path      = lv_path
              iv_val       = <node_b>-value
              iv_node_type = <node_b>-type ).
          endif.
      endcase.
    endloop.

  endmethod.


  method sort.

    data lo_json type ref to zif_ajson.

    lo_json = check_input(
      iv_json = iv_json
      io_json = io_json ).

    " Nodes are parsed into a sorted table, so no explicit sorting required
    rv_sorted = lo_json->stringify( 2 ).

  endmethod.
ENDCLASS.
