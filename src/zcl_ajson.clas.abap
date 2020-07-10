class zcl_ajson definition
  public
  create private .

  public section.

    interfaces zif_ajson_reader .
*    aliases: " NOT SURE
*      exists for zif_ajson_reader~exists,
*      members for zif_ajson_reader~members,
*      value for zif_ajson_reader~value,
*      value_boolean for zif_ajson_reader~value_boolean,
*      value_integer for zif_ajson_reader~value_integer,
*      value_number for zif_ajson_reader~value_number,
*      value_string for zif_ajson_reader~value_string,
*      slice for zif_ajson_reader~slice,
*      to_abap for zif_ajson_reader~to_abap.

    interfaces zif_ajson_writer .
*    aliases: " NOT SURE
*      clear for zif_ajson_writer~clear,
*      set for zif_ajson_writer~set,
*      set_boolean for zif_ajson_writer~set_boolean,
*      set_string for zif_ajson_writer~set_string,
*      set_integer for zif_ajson_writer~set_integer,
*      set_date for zif_ajson_writer~set_date,
*      delete for zif_ajson_writer~delete,
*      touch_array for zif_ajson_writer~touch_array,
*      push for zif_ajson_writer~push.

    types:
      begin of ty_node,
        path type string,
        name type string,
        type type string,
        value type string,
        index type i,
        children type i,
      end of ty_node .
    types:
      ty_nodes_tt type standard table of ty_node with key path name .
    types:
      ty_nodes_ts type sorted table of ty_node
        with unique key path name
        with non-unique sorted key array_index components path index .
    types:
      begin of ty_path_name,
        path type string,
        name type string,
      end of ty_path_name.

    class-methods parse
      importing
        !iv_json type string
      returning
        value(ro_instance) type ref to zcl_ajson
      raising
        zcx_ajson_error .

    class-methods create_empty
      returning
        value(ro_instance) type ref to zcl_ajson.

    methods stringify
      returning
        value(rv_json) type string.

  protected section.

  private section.

    types:
      tty_node_stack type standard table of ref to ty_node with default key.

    data mt_json_tree type ty_nodes_ts.

    methods get_item
      importing
        iv_path type string
      returning
        value(rv_item) type ref to ty_node.
    methods prove_path_exists
      importing
        iv_path type string
      returning
        value(rt_node_stack) type tty_node_stack.
    methods delete_subtree
      importing
        iv_path type string
        iv_name type string
      returning
        value(rv_deleted) type abap_bool.

ENDCLASS.



CLASS ZCL_AJSON IMPLEMENTATION.


  method create_empty.
    create object ro_instance.
  endmethod.


  method delete_subtree.

    data lv_parent_path type string.
    data lv_parent_path_len type i.
    field-symbols <node> like line of mt_json_tree.
    read table mt_json_tree assigning <node>
      with key
        path = iv_path
        name = iv_name.
    if sy-subrc = 0. " Found ? delete !
      if <node>-children > 0. " only for objects and arrays
        lv_parent_path = iv_path && iv_name && '/'.
        lv_parent_path_len = strlen( lv_parent_path ).
        loop at mt_json_tree assigning <node>.
          if strlen( <node>-path ) >= lv_parent_path_len
            and substring( val = <node>-path len = lv_parent_path_len ) = lv_parent_path.
            delete mt_json_tree index sy-tabix.
          endif.
        endloop.
      endif.

      delete mt_json_tree where path = iv_path and name = iv_name.
      rv_deleted = abap_true.

      data ls_path type ty_path_name.
      ls_path = lcl_utils=>split_path( iv_path ).
      read table mt_json_tree assigning <node>
        with key
          path = ls_path-path
          name = ls_path-name.
      if sy-subrc = 0.
        <node>-children = <node>-children - 1.
      endif.
    endif.

  endmethod.


  method get_item.

    field-symbols <item> like line of mt_json_tree.
    data ls_path_name type ty_path_name.
    ls_path_name = lcl_utils=>split_path( iv_path ).

    read table mt_json_tree
      assigning <item>
      with key
        path = ls_path_name-path
        name = ls_path_name-name.
    if sy-subrc = 0.
      get reference of <item> into rv_item.
    endif.

  endmethod.


  method parse.

    data lo_parser type ref to lcl_json_parser.

    create object ro_instance.
    create object lo_parser.
    ro_instance->mt_json_tree = lo_parser->parse( iv_json ).

  endmethod.


  method prove_path_exists.

    data lt_path type string_table.
    data node_ref like line of rt_node_stack.
    data lv_size type i.
    data lv_cur_path type string.
    data lv_cur_name type string.
    data node_tmp like line of mt_json_tree.

    split iv_path at '/' into table lt_path.
    delete lt_path where table_line is initial.
    lv_size = lines( lt_path ).

    do.
      read table mt_json_tree reference into node_ref
        with key
          path = lv_cur_path
          name = lv_cur_name.
      if sy-subrc <> 0. " New node, assume it is always object as it has a named child, use touch_array to init array
        if node_ref is not initial. " if has parent
          node_ref->children = node_ref->children + 1.
        endif.
        node_tmp-path = lv_cur_path.
        node_tmp-name = lv_cur_name.
        node_tmp-type = 'object'.
        insert node_tmp into table mt_json_tree reference into node_ref.
      endif.
      insert node_ref into rt_node_stack index 1.
      lv_cur_path = lv_cur_path && lv_cur_name && '/'.
      read table lt_path index sy-index into lv_cur_name.
      if sy-subrc <> 0.
        exit. " no more segments
      endif.
    enddo.

    assert lv_cur_path = iv_path. " Just in case

  endmethod.


  method stringify.
  endmethod.


  method zif_ajson_reader~exists.

    data lv_item type ref to ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial.
      rv_exists = abap_true.
    endif.

  endmethod.


  method zif_ajson_reader~members.

    data lv_normalized_path type string.
    field-symbols <item> like line of mt_json_tree.

    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).

    loop at mt_json_tree assigning <item> where path = lv_normalized_path.
      append <item>-name to rt_members.
    endloop.

  endmethod.


  method zif_ajson_reader~slice.

    data lo_section         type ref to zcl_ajson.
    data ls_item            like line of mt_json_tree.
    data lv_normalized_path type string.
    data ls_path_parts      type ty_path_name.
    data lv_path_len        type i.

    create object lo_section.
    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).
    lv_path_len        = strlen( lv_normalized_path ).
    ls_path_parts      = lcl_utils=>split_path( lv_normalized_path ).

    loop at mt_json_tree into ls_item.
      " TODO potentially improve performance due to sorted tree (all path started from same prefix go in a row)
      if strlen( ls_item-path ) >= lv_path_len
          and substring( val = ls_item-path len = lv_path_len ) = lv_normalized_path.
        ls_item-path = substring( val = ls_item-path off = lv_path_len - 1 ). " less closing '/'
        insert ls_item into table lo_section->mt_json_tree.
      elseif ls_item-path = ls_path_parts-path and ls_item-name = ls_path_parts-name.
        clear: ls_item-path, ls_item-name. " this becomes a new root
        insert ls_item into table lo_section->mt_json_tree.
      endif.
    endloop.

    ri_json = lo_section.

  endmethod.


  method zif_ajson_reader~to_abap.

    data lo_to_abap type ref to lcl_json_to_abap.

    clear ev_container.
    lcl_json_to_abap=>bind(
      changing
        c_obj = ev_container
        co_instance = lo_to_abap ).
    lo_to_abap->to_abap( mt_json_tree ).

  endmethod.


  method zif_ajson_reader~value.

    data lv_item type ref to ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial.
      rv_value = lv_item->value.
    endif.

  endmethod.


  method zif_ajson_reader~value_boolean.

    data lv_item type ref to ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is initial or lv_item->type = 'null'.
      return.
    elseif lv_item->type = 'bool'.
      rv_value = boolc( lv_item->value = 'true' ).
    elseif lv_item->value is not initial.
      rv_value = abap_true.
    endif.

  endmethod.


  method zif_ajson_reader~value_integer.

    data lv_item type ref to ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial and lv_item->type = 'num'.
      rv_value = lv_item->value.
    endif.

  endmethod.


  method zif_ajson_reader~value_number.

    data lv_item type ref to ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial and lv_item->type = 'num'.
      rv_value = lv_item->value.
    endif.

  endmethod.


  method zif_ajson_reader~value_string.

    data lv_item type ref to ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial and lv_item->type <> 'null'.
      rv_value = lv_item->value.
    endif.

  endmethod.


  method zif_ajson_writer~clear.
    clear mt_json_tree.
  endmethod.


  method zif_ajson_writer~delete.

    data ls_split_path type ty_path_name.
    ls_split_path = lcl_utils=>split_path( iv_path ).

    delete_subtree(
      iv_path = ls_split_path-path
      iv_name = ls_split_path-name ).

  endmethod.


  method zif_ajson_writer~push.

    data parent_ref type ref to ty_node.
    data new_node_ref type ref to ty_node.

    parent_ref = get_item( iv_path ).

    if parent_ref is initial.
      raise exception type zcx_ajson_error
        exporting
          message = |Path [{ iv_path }] does not exist|.
    endif.

    if parent_ref->type <> 'array'.
      raise exception type zcx_ajson_error
        exporting
          message = |Path [{ iv_path }] is not array|.
    endif.

    data lt_new_nodes type ty_nodes_tt.
    data ls_new_path type ty_path_name.

    ls_new_path-path = lcl_utils=>normalize_path( iv_path ).
    ls_new_path-name = |{ parent_ref->children + 1 }|.

    lt_new_nodes = lcl_abap_to_json=>convert(
      iv_data   = iv_val
      is_prefix = ls_new_path ).
    read table lt_new_nodes index 1 reference into new_node_ref. " assume first record is the array item - not ideal !
    assert sy-subrc = 0.
    new_node_ref->index = parent_ref->children + 1.

    " update data
    parent_ref->children = parent_ref->children + 1.
    insert lines of lt_new_nodes into table mt_json_tree.

  endmethod.


  method zif_ajson_writer~set.

    data lt_path type string_table.
    data ls_split_path type ty_path_name.
    data parent_ref type ref to ty_node.
    data lt_node_stack type table of ref to ty_node.

    if iv_val is initial and iv_ignore_empty = abap_true.
      return. " nothing to assign
    endif.

    ls_split_path = lcl_utils=>split_path( iv_path ).
    if ls_split_path is initial. " Assign root, exceptional processing
      mt_json_tree = lcl_abap_to_json=>convert(
        iv_data   = iv_val
        is_prefix = ls_split_path ).
      return.
    endif.

    " Ensure whole path exists
    lt_node_stack = prove_path_exists( ls_split_path-path ).
    read table lt_node_stack index 1 into parent_ref.
    assert sy-subrc = 0.

    " delete if exists with subtree
    delete_subtree(
      iv_path = ls_split_path-path
      iv_name = ls_split_path-name ).

    " convert to json
    data lt_new_nodes type ty_nodes_tt.
    lt_new_nodes = lcl_abap_to_json=>convert(
      iv_data   = iv_val
      is_prefix = ls_split_path ).

    " update data
    parent_ref->children = parent_ref->children + 1.
    insert lines of lt_new_nodes into table mt_json_tree.

  endmethod.


  method zif_ajson_writer~set_boolean.

    data lv_bool type abap_bool.
    lv_bool = boolc( iv_val is not initial ).
    zif_ajson_writer~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_bool ).

  endmethod.


  method zif_ajson_writer~set_date.

    data lv_val type string.

    if iv_val is not initial.
      lv_val = iv_val+0(4) && '-' && iv_val+4(2) && '-' && iv_val+6(2).
    endif.

    zif_ajson_writer~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_val ).

  endmethod.


  method zif_ajson_writer~set_integer.

    zif_ajson_writer~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = iv_val ).

  endmethod.


  method zif_ajson_writer~set_null.

    data lv_null_ref type ref to data.
    zif_ajson_writer~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_null_ref ).

  endmethod.


  method zif_ajson_writer~set_string.

    data lv_val type string.
    lv_val = iv_val.
    zif_ajson_writer~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_val ).

  endmethod.


  method zif_ajson_writer~touch_array.

    data node_ref type ref to ty_node.
    data ls_new_node like line of mt_json_tree.
    data ls_split_path type ty_path_name.

    ls_split_path = lcl_utils=>split_path( iv_path ).
    if ls_split_path is initial. " Assign root, exceptional processing
      ls_new_node-path = ls_split_path-path.
      ls_new_node-name = ls_split_path-name.
      ls_new_node-type = 'array'.
      insert ls_new_node into table mt_json_tree.
      return.
    endif.

    if iv_clear = abap_true.
      delete_subtree(
        iv_path = ls_split_path-path
        iv_name = ls_split_path-name ).
    else.
      node_ref = get_item( iv_path ).
    endif.

    if node_ref is initial. " Or node was cleared

      data parent_ref type ref to ty_node.
      data lt_node_stack type table of ref to ty_node.

      lt_node_stack = prove_path_exists( ls_split_path-path ).
      read table lt_node_stack index 1 into parent_ref.
      assert sy-subrc = 0.
      parent_ref->children = parent_ref->children + 1.

      ls_new_node-path = ls_split_path-path.
      ls_new_node-name = ls_split_path-name.
      ls_new_node-type = 'array'.
      insert ls_new_node into table mt_json_tree.

    elseif node_ref->type <> 'array'.
      raise exception type zcx_ajson_error
        exporting
          message = |Path [{ iv_path }] already used and is not array|.
    endif.

  endmethod.
ENDCLASS.
