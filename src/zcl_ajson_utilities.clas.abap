class zcl_ajson_utilities definition
  public
  create private .

  public section.

    methods diff
      importing
        !iv_json_a       type string optional
        !iv_json_b       type string optional
        !io_json_a       type ref to zcl_ajson optional
        !io_json_b       type ref to zcl_ajson optional
      exporting
        !eo_insert       type ref to zcl_ajson
        !eo_delete       type ref to zcl_ajson
        !eo_change       type ref to zcl_ajson
      raising
        zcx_ajson_error .

  protected section.

  private section.

    data:
      mo_json_a type ref to zcl_ajson,
      mo_json_b type ref to zcl_ajson,
      mo_insert type ref to zif_ajson_writer,
      mo_delete type ref to zif_ajson_writer,
      mo_change type ref to zif_ajson_writer.

    methods diff_a_b
      importing
        iv_path type string
      raising
        zcx_ajson_error.

    methods diff_b_a
      importing
        iv_path type string
      raising
        zcx_ajson_error.

    methods delete_empty_arrays
      importing
        io_json type ref to zcl_ajson
      raising
        zcx_ajson_error.

ENDCLASS.



CLASS zcl_ajson_utilities IMPLEMENTATION.


  method delete_empty_arrays.

    data ls_json_tree type zcl_ajson=>ty_node.

    loop at io_json->mt_json_tree into ls_json_tree
      where type = 'array' and children = 0.

      io_json->delete( ls_json_tree-path && '/' && ls_json_tree-name ).

    endloop.

  endmethod.


  method diff.

    if iv_json_a is supplied and io_json_a is supplied.
      zcx_ajson_error=>raise( 'Either supply JSON string or instance, but not both' ).
    endif.
    if iv_json_b is supplied and io_json_b is supplied.
      zcx_ajson_error=>raise( 'Either supply JSON string or instance, but not both' ).
    endif.

    if iv_json_a is supplied.
      mo_json_a = zcl_ajson=>parse( iv_json_a ).
    elseif io_json_a is bound.
      mo_json_a = io_json_a.
    else.
      zcx_ajson_error=>raise( 'Supply either JSON string or instance' ).
    endif.

    if iv_json_b is supplied.
      mo_json_b = zcl_ajson=>parse( iv_json_b ).
    elseif io_json_a is bound.
      mo_json_b = io_json_b.
    else.
      zcx_ajson_error=>raise( 'Supply either JSON string or instance' ).
    endif.

    mo_insert = zcl_ajson=>create_empty( ).
    mo_delete = zcl_ajson=>create_empty( ).
    mo_change = zcl_ajson=>create_empty( ).

    diff_a_b( iv_path = '/' ).
    diff_b_a( iv_path = '/' ).

    eo_insert ?= mo_insert.
    eo_delete ?= mo_delete.
    eo_change ?= mo_change.

    delete_empty_arrays( eo_insert ).
    delete_empty_arrays( eo_delete ).
    delete_empty_arrays( eo_change ).

  endmethod.


  method diff_a_b.

    data lv_path type string.

    field-symbols:
      <ls_node_a> type zcl_ajson=>ty_node,
      <ls_node_b> type zcl_ajson=>ty_node.

    loop at mo_json_a->mt_json_tree assigning <ls_node_a> where path = iv_path.
      lv_path = <ls_node_a>-path && <ls_node_a>-name && '/'.

      case <ls_node_a>-type.
        when 'array'.
          mo_change->touch_array( lv_path ).
          mo_delete->touch_array( lv_path ).
          diff_a_b( iv_path = lv_path ).
        when 'object'.
          diff_a_b( iv_path = lv_path ).
        when others.
          read table mo_json_b->mt_json_tree assigning <ls_node_b>
            with table key path = <ls_node_a>-path name = <ls_node_a>-name.
          if sy-subrc = 0.
            if <ls_node_a>-type <> <ls_node_b>-type or <ls_node_a>-value <> <ls_node_b>-value.
              " save as change
              mo_change->set_with_type( iv_path = <ls_node_b>-path && <ls_node_b>-name && '/'
                                        iv_val  = <ls_node_b>-value
                                        iv_type = <ls_node_b>-type ).
            endif.
          else.
            " save as delete
            mo_delete->set_with_type( iv_path = lv_path
                                      iv_val  = <ls_node_a>-value
                                      iv_type = <ls_node_a>-type ).
          endif.
      endcase.
    endloop.

  endmethod.


  method diff_b_a.

    data lv_path type string.

    field-symbols:
      <ls_node_a> type zcl_ajson=>ty_node,
      <ls_node_b> type zcl_ajson=>ty_node.

    loop at mo_json_b->mt_json_tree assigning <ls_node_b> where path = iv_path.
      lv_path = <ls_node_b>-path && <ls_node_b>-name && '/'.

      case <ls_node_b>-type.
        when 'array'.
          mo_insert->touch_array( lv_path ).
          diff_b_a( iv_path = lv_path ).
        when 'object'.
          diff_b_a( iv_path = lv_path ).
        when others.
          read table mo_json_a->mt_json_tree assigning <ls_node_a>
            with table key path = <ls_node_b>-path name = <ls_node_b>-name.
          if sy-subrc <> 0.
            " save as insert
            mo_insert->set_with_type( iv_path = <ls_node_b>-path && <ls_node_b>-name && '/'
                                      iv_val  = <ls_node_b>-value
                                      iv_type = <ls_node_b>-type ).
          endif.
      endcase.
    endloop.

  endmethod.
ENDCLASS.
