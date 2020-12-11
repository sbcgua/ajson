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

  PRIVATE SECTION.

    DATA mo_json_a TYPE REF TO zcl_ajson .
    DATA mo_json_b TYPE REF TO zcl_ajson .
    DATA mo_insert TYPE REF TO zif_ajson_writer .
    DATA mo_delete TYPE REF TO zif_ajson_writer .
    DATA mo_change TYPE REF TO zif_ajson_writer .

    METHODS diff_a_b
      IMPORTING
        !iv_path TYPE string
      RAISING
        zcx_ajson_error .
    METHODS diff_b_a
      IMPORTING
        !iv_path TYPE string
      RAISING
        zcx_ajson_error .
    METHODS delete_empty_nodes
      IMPORTING
        !io_json TYPE REF TO zcl_ajson
      RAISING
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
      <node_a> type zcl_ajson=>ty_node,
      <node_b> type zcl_ajson=>ty_node.

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
      <node_a> type zcl_ajson=>ty_node,
      <node_b> type zcl_ajson=>ty_node.

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
    endif.

    " Nodes are parsed into a sorted table, so no explicit sorting required
    rv_sorted = lo_json->stringify( 2 ).

  endmethod.
ENDCLASS.
