class zcl_ajson_utilities definition
  public
  create public .

  public section.

    methods diff
      importing
        !iv_json_a type string optional
        !iv_json_b type string optional
        !io_json_a type ref to zcl_ajson optional
        !io_json_b type ref to zcl_ajson optional
      exporting
        !eo_insert type ref to zcl_ajson
        !eo_delete type ref to zcl_ajson
        !eo_change type ref to zcl_ajson
      raising
        zcx_ajson_error .
    methods sort
      importing
        !iv_json         type string optional
        !io_json         type ref to zcl_ajson optional
      returning
        value(rv_sorted) type string
      raising
        zcx_ajson_error .
  protected section.

  private section.

    data mo_json_a type ref to zcl_ajson .
    data mo_json_b type ref to zcl_ajson .
    data mo_insert type ref to zif_ajson_writer .
    data mo_delete type ref to zif_ajson_writer .
    data mo_change type ref to zif_ajson_writer .

    methods diff_a_b
      importing
        !iv_path type string
      raising
        zcx_ajson_error .
    methods diff_b_a
      importing
        !iv_path type string
        !iv_all  type abap_bool default abap_false
      raising
        zcx_ajson_error .
    methods delete_empty_nodes
      importing
        !io_json type ref to zcl_ajson
      raising
        zcx_ajson_error .
ENDCLASS.



CLASS zcl_ajson_utilities IMPLEMENTATION.


  method delete_empty_nodes.

    data ls_json_tree type zcl_ajson=>ty_node.
    data lv_subrc type sy-subrc.

    do.
      loop at io_json->mt_json_tree into ls_json_tree
        where type = 'array' and children = 0.

        io_json->delete( ls_json_tree-path && ls_json_tree-name ).

      endloop.
      lv_subrc = sy-subrc.

      loop at io_json->mt_json_tree into ls_json_tree
        where type = 'object' and children = 0.

        io_json->delete( ls_json_tree-path && ls_json_tree-name ).

      endloop.
      if lv_subrc = 4 and sy-subrc = 4.
        exit. " nothing else to delete
      endif.
    enddo.

  endmethod.


  method diff.

    if boolc( iv_json_a is supplied ) = boolc( io_json_a is supplied ).
      zcx_ajson_error=>raise( 'Either supply JSON string or instance, but not both' ).
    endif.
    if boolc( iv_json_b is supplied ) = boolc( io_json_b is supplied ).
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

    diff_a_b( '/' ).
    diff_b_a( '/' ).

    eo_insert ?= mo_insert.
    eo_delete ?= mo_delete.
    eo_change ?= mo_change.

    delete_empty_nodes( eo_insert ).
    delete_empty_nodes( eo_delete ).
    delete_empty_nodes( eo_change ).

  endmethod.


  method diff_a_b.

    data:
      lv_path_a type string,
      lv_path_b type string.

    field-symbols:
      <ls_node_a> type zcl_ajson=>ty_node,
      <ls_node_b> type zcl_ajson=>ty_node.

    loop at mo_json_a->mt_json_tree assigning <ls_node_a> where path = iv_path.
      lv_path_a = <ls_node_a>-path && <ls_node_a>-name && '/'.

      case <ls_node_a>-type.
        when 'array'.
          mo_change->touch_array( lv_path_a ).
          mo_delete->touch_array( lv_path_a ).
          diff_a_b( lv_path_a ).
        when 'object'.
          diff_a_b( lv_path_a ).
        when others.
          read table mo_json_b->mt_json_tree assigning <ls_node_b>
            with table key path = <ls_node_a>-path name = <ls_node_a>-name.
          if sy-subrc = 0.
            lv_path_b = <ls_node_b>-path && <ls_node_b>-name && '/'.

            if <ls_node_a>-type = <ls_node_b>-type and <ls_node_a>-value <> <ls_node_b>-value.
              " save as changed value
              mo_change->set(
                iv_path      = lv_path_b
                iv_val       = <ls_node_b>-value
                iv_node_type = <ls_node_b>-type ).
            elseif <ls_node_a>-type <> <ls_node_b>-type.
              " save changed type as delete + insert
              mo_delete->set(
                iv_path      = lv_path_a
                iv_val       = <ls_node_a>-value
                iv_node_type = <ls_node_a>-type ).
              mo_insert->set(
                iv_path      = lv_path_b
                iv_val       = <ls_node_b>-value
                iv_node_type = <ls_node_b>-type ).
              " new type might have sub-nodes
              diff_b_a( lv_path_b ).
            endif.
          else.
            " save as delete
            mo_delete->set(
              iv_path      = lv_path_a
              iv_val       = <ls_node_a>-value
              iv_node_type = <ls_node_a>-type ).
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
          diff_b_a( lv_path ).
        when 'object'.
          diff_b_a( lv_path ).
        when others.
          read table mo_json_a->mt_json_tree assigning <ls_node_a>
            with table key path = <ls_node_b>-path name = <ls_node_b>-name.
          if sy-subrc <> 0 or iv_all = abap_true.
            " save as insert
            mo_insert->set(
              iv_path      = <ls_node_b>-path && <ls_node_b>-name && '/'
              iv_val       = <ls_node_b>-value
              iv_node_type = <ls_node_b>-type ).
          endif.
      endcase.
    endloop.

  endmethod.


  method sort.

    data lo_json type ref to zcl_ajson.

    if boolc( iv_json is supplied ) = boolc( io_json is supplied ).
      zcx_ajson_error=>raise( 'Either supply JSON string or instance, but not both' ).
    endif.

    if iv_json is supplied.
      lo_json = zcl_ajson=>parse( iv_json ).
    elseif io_json is bound.
      lo_json = io_json.
    else.
      zcx_ajson_error=>raise( 'Supply either JSON string or instance' ).
    ENDIF.

    " Nodes are parsed into a sorted table, so no explicit sorting required
    rv_sorted = lo_json->stringify( 2 ).

  endmethod.
ENDCLASS.
