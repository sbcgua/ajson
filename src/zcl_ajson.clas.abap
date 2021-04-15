class zcl_ajson definition
  public
  create private .

  public section.

    interfaces zif_ajson_reader .
    interfaces zif_ajson_writer .
    interfaces zif_ajson .

    aliases:
      exists for zif_ajson_reader~exists,
      members for zif_ajson_reader~members,
      get for zif_ajson_reader~get,
      get_boolean for zif_ajson_reader~get_boolean,
      get_integer for zif_ajson_reader~get_integer,
      get_number for zif_ajson_reader~get_number,
      get_date for zif_ajson_reader~get_date,
      get_timestamp for zif_ajson_reader~get_timestamp,
      get_string for zif_ajson_reader~get_string,
      slice for zif_ajson_reader~slice,
      to_abap for zif_ajson_reader~to_abap,
      array_to_string_table for zif_ajson_reader~array_to_string_table.

    aliases:
      clear for zif_ajson_writer~clear,
      set for zif_ajson_writer~set,
      set_boolean for zif_ajson_writer~set_boolean,
      set_string for zif_ajson_writer~set_string,
      set_integer for zif_ajson_writer~set_integer,
      set_date for zif_ajson_writer~set_date,
      set_null for zif_ajson_writer~set_null,
      delete for zif_ajson_writer~delete,
      touch_array for zif_ajson_writer~touch_array,
      push for zif_ajson_writer~push,
      stringify for zif_ajson_writer~stringify.

    aliases:
      mt_json_tree for zif_ajson~mt_json_tree,
      keep_item_order for zif_ajson~keep_item_order,
      freeze for zif_ajson~freeze.

    class-methods parse
      importing
        !iv_json           type string
        !iv_freeze         type abap_bool default abap_false
        !ii_custom_mapping type ref to zif_ajson_mapping optional
      returning
        value(ro_instance) type ref to zcl_ajson
      raising
        zcx_ajson_error .

    class-methods create_empty
      importing
        !ii_custom_mapping type ref to zif_ajson_mapping optional
      returning
        value(ro_instance) type ref to zcl_ajson.

  protected section.

  private section.

    types:
      tty_node_stack type standard table of ref to zif_ajson=>ty_node with default key.

    data mv_read_only type abap_bool.
    data mi_custom_mapping type ref to zif_ajson_mapping.
    data mv_keep_item_order type abap_bool.

    methods get_item
      importing
        iv_path        type string
      returning
        value(rv_item) type ref to zif_ajson=>ty_node.
    methods prove_path_exists
      importing
        iv_path              type string
      returning
        value(rt_node_stack) type tty_node_stack
      raising
        zcx_ajson_error.
    methods delete_subtree
      importing
        iv_path           type string
        iv_name           type string
      returning
        value(rv_deleted) type abap_bool.

ENDCLASS.



CLASS ZCL_AJSON IMPLEMENTATION.


  method create_empty.
    create object ro_instance.
    ro_instance->mi_custom_mapping = ii_custom_mapping.
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

      data ls_path type zif_ajson=>ty_path_name.
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
    data ls_path_name type zif_ajson=>ty_path_name.
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
    ro_instance->mi_custom_mapping = ii_custom_mapping.

    if iv_freeze = abap_true.
      ro_instance->freeze( ).
    endif.

  endmethod.


  method prove_path_exists.

    data lt_path type string_table.
    data lr_node like line of rt_node_stack.
    data lr_node_parent like line of rt_node_stack.
    data lv_cur_path type string.
    data lv_cur_name type string.
    data ls_new_node like line of mt_json_tree.

    split iv_path at '/' into table lt_path.
    delete lt_path where table_line is initial.

    do.
      lr_node_parent = lr_node.
      read table mt_json_tree reference into lr_node
        with key
          path = lv_cur_path
          name = lv_cur_name.
      if sy-subrc <> 0. " New node, assume it is always object as it has a named child, use touch_array to init array
        clear ls_new_node.
        if lr_node_parent is not initial. " if has parent
          lr_node_parent->children = lr_node_parent->children + 1.
          if lr_node_parent->type = zif_ajson=>node_type-array.
            ls_new_node-index = lcl_utils=>validate_array_index(
              iv_path  = lv_cur_path
              iv_index = lv_cur_name ).
          endif.
        endif.
        ls_new_node-path = lv_cur_path.
        ls_new_node-name = lv_cur_name.
        ls_new_node-type = zif_ajson=>node_type-object.
        insert ls_new_node into table mt_json_tree reference into lr_node.
      endif.
      insert lr_node into rt_node_stack index 1.
      lv_cur_path = lv_cur_path && lv_cur_name && '/'.
      read table lt_path index sy-index into lv_cur_name.
      if sy-subrc <> 0.
        exit. " no more segments
      endif.
    enddo.

    assert lv_cur_path = iv_path. " Just in case

  endmethod.


  method zif_ajson_reader~array_to_string_table.

    data lv_normalized_path type string.
    data lr_node type ref to zif_ajson=>ty_node.
    field-symbols <item> like line of mt_json_tree.

    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).
    lr_node = get_item( iv_path ).

    if lr_node is initial.
      zcx_ajson_error=>raise( |Path not found: { iv_path }| ).
    endif.
    if lr_node->type <> zif_ajson=>node_type-array.
      zcx_ajson_error=>raise( |Array expected at: { iv_path }| ).
    endif.

    loop at mt_json_tree assigning <item> where path = lv_normalized_path.
      case <item>-type.
        when zif_ajson=>node_type-number or zif_ajson=>node_type-string.
          append <item>-value to rt_string_table.
        when zif_ajson=>node_type-null.
          append '' to rt_string_table.
        when zif_ajson=>node_type-boolean.
          data lv_tmp type string.
          if <item>-value = 'true'.
            lv_tmp = abap_true.
          else.
            clear lv_tmp.
          endif.
          append lv_tmp to rt_string_table.
        when others.
          zcx_ajson_error=>raise( |Cannot convert [{ <item>-type
            }] to string at [{ <item>-path }{ <item>-name }]| ).
      endcase.
    endloop.

  endmethod.


  method zif_ajson_reader~exists.

    data lv_item type ref to zif_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial.
      rv_exists = abap_true.
    endif.

  endmethod.


  method zif_ajson_reader~get.

    data lv_item type ref to zif_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial.
      rv_value = lv_item->value.
    endif.

  endmethod.


  method zif_ajson_reader~get_boolean.

    data lv_item type ref to zif_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is initial or lv_item->type = zif_ajson=>node_type-null.
      return.
    elseif lv_item->type = zif_ajson=>node_type-boolean.
      rv_value = boolc( lv_item->value = 'true' ).
    elseif lv_item->value is not initial.
      rv_value = abap_true.
    endif.

  endmethod.


  method zif_ajson_reader~get_date.

    data lv_item type ref to zif_ajson=>ty_node.
    data lv_y type c length 4.
    data lv_m type c length 2.
    data lv_d type c length 2.

    lv_item = get_item( iv_path ).

    if lv_item is not initial and lv_item->type = zif_ajson=>node_type-string.
      find first occurrence of regex '^(\d{4})-(\d{2})-(\d{2})(T|$)'
        in lv_item->value
        submatches lv_y lv_m lv_d.
      concatenate lv_y lv_m lv_d into rv_value.
    endif.

  endmethod.


  method zif_ajson_reader~get_integer.

    data lv_item type ref to zif_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial and lv_item->type = zif_ajson=>node_type-number.
      rv_value = lv_item->value.
    endif.

  endmethod.


  method zif_ajson_reader~get_node_type.

    data lv_item type ref to zif_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial.
      rv_node_type = lv_item->type.
    endif.

  endmethod.


  method zif_ajson_reader~get_number.

    data lv_item type ref to zif_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial and lv_item->type = zif_ajson=>node_type-number.
      rv_value = lv_item->value.
    endif.

  endmethod.


  method zif_ajson_reader~get_string.

    data lv_item type ref to zif_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial and lv_item->type <> zif_ajson=>node_type-null.
      rv_value = lv_item->value.
    endif.

  endmethod.


  method zif_ajson_reader~get_timestamp.

    data lo_to_abap type ref to lcl_json_to_abap.
    data lr_item type ref to zif_ajson=>ty_node.

    lr_item = get_item( iv_path ).

    create object lo_to_abap.

    try.
      rv_value = lo_to_abap->to_timestamp( is_path = lr_item->* ).
    catch zcx_ajson_error.
      return.
    endtry.

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
    data ls_path_parts      type zif_ajson=>ty_path_name.
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
      exporting
        ii_custom_mapping = mi_custom_mapping
      changing
        c_obj             = ev_container
        co_instance       = lo_to_abap ).
    lo_to_abap->to_abap( mt_json_tree ).

  endmethod.


  method zif_ajson_writer~clear.

    if mv_read_only = abap_true.
      zcx_ajson_error=>raise( 'This json instance is read only' ).
    endif.

    clear mt_json_tree.

  endmethod.


  method zif_ajson_writer~delete.

    if mv_read_only = abap_true.
      zcx_ajson_error=>raise( 'This json instance is read only' ).
    endif.

    data ls_split_path type zif_ajson=>ty_path_name.
    ls_split_path = lcl_utils=>split_path( iv_path ).

    delete_subtree(
      iv_path = ls_split_path-path
      iv_name = ls_split_path-name ).

  endmethod.


  method zif_ajson_writer~push.

    data lr_parent type ref to zif_ajson=>ty_node.
    data lr_new_node type ref to zif_ajson=>ty_node.

    if mv_read_only = abap_true.
      zcx_ajson_error=>raise( 'This json instance is read only' ).
    endif.

    lr_parent = get_item( iv_path ).

    if lr_parent is initial.
      zcx_ajson_error=>raise( |Path [{ iv_path }] does not exist| ).
    endif.

    if lr_parent->type <> zif_ajson=>node_type-array.
      zcx_ajson_error=>raise( |Path [{ iv_path }] is not array| ).
    endif.

    data lt_new_nodes type zif_ajson=>ty_nodes_tt.
    data ls_new_path type zif_ajson=>ty_path_name.

    ls_new_path-path = lcl_utils=>normalize_path( iv_path ).
    ls_new_path-name = |{ lr_parent->children + 1 }|.

    lt_new_nodes = lcl_abap_to_json=>convert(
      iv_keep_item_order = mv_keep_item_order
      iv_data   = iv_val
      is_prefix = ls_new_path ).
    read table lt_new_nodes index 1 reference into lr_new_node. " assume first record is the array item - not ideal !
    assert sy-subrc = 0.
    lr_new_node->index = lr_parent->children + 1.

    " update data
    lr_parent->children = lr_parent->children + 1.
    insert lines of lt_new_nodes into table mt_json_tree.

  endmethod.


  method zif_ajson_writer~set.

    data ls_split_path type zif_ajson=>ty_path_name.
    data lr_parent type ref to zif_ajson=>ty_node.
    data lt_node_stack type tty_node_stack.

    if mv_read_only = abap_true.
      zcx_ajson_error=>raise( 'This json instance is read only' ).
    endif.

    if iv_val is initial and iv_ignore_empty = abap_true and iv_node_type is initial.
      return. " nothing to assign
    endif.

    if iv_node_type is not initial
      and iv_node_type <> zif_ajson=>node_type-boolean and iv_node_type <> zif_ajson=>node_type-null
      and iv_node_type <> zif_ajson=>node_type-number and iv_node_type <> zif_ajson=>node_type-string.
      zcx_ajson_error=>raise( |Unexpected type { iv_node_type }| ).
    endif.

    ls_split_path = lcl_utils=>split_path( iv_path ).
    if ls_split_path is initial. " Assign root, exceptional processing
      if iv_node_type is not initial.
        mt_json_tree = lcl_abap_to_json=>insert_with_type(
          iv_keep_item_order = mv_keep_item_order
          iv_data            = iv_val
          iv_type            = iv_node_type
          is_prefix          = ls_split_path
          ii_custom_mapping  = mi_custom_mapping ).
      else.
        mt_json_tree = lcl_abap_to_json=>convert(
          iv_keep_item_order = mv_keep_item_order
          iv_data            = iv_val
          is_prefix          = ls_split_path
          ii_custom_mapping  = mi_custom_mapping ).
      endif.
      return.
    endif.

    " Ensure whole path exists
    lt_node_stack = prove_path_exists( ls_split_path-path ).
    read table lt_node_stack index 1 into lr_parent.
    assert sy-subrc = 0.

    " delete if exists with subtree
    delete_subtree(
      iv_path = ls_split_path-path
      iv_name = ls_split_path-name ).

    " convert to json
    data lt_new_nodes type zif_ajson=>ty_nodes_tt.
    data lv_array_index type i.

    if lr_parent->type = zif_ajson=>node_type-array.
      lv_array_index = lcl_utils=>validate_array_index(
        iv_path  = ls_split_path-path
        iv_index = ls_split_path-name ).
    endif.

    if iv_node_type is not initial.
      lt_new_nodes = lcl_abap_to_json=>insert_with_type(
        iv_keep_item_order = mv_keep_item_order
        iv_data            = iv_val
        iv_type            = iv_node_type
        iv_array_index     = lv_array_index
        is_prefix          = ls_split_path
        ii_custom_mapping  = mi_custom_mapping ).
    else.
      lt_new_nodes = lcl_abap_to_json=>convert(
        iv_keep_item_order = mv_keep_item_order
        iv_data            = iv_val
        iv_array_index     = lv_array_index
        is_prefix          = ls_split_path
        ii_custom_mapping  = mi_custom_mapping ).
    endif.

    " update data
    lr_parent->children = lr_parent->children + 1.
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


  method zif_ajson_writer~stringify.

    rv_json = lcl_json_serializer=>stringify(
      it_json_tree       = mt_json_tree
      iv_keep_item_order = mv_keep_item_order
      iv_indent          = iv_indent ).

  endmethod.


  method zif_ajson_writer~touch_array.

    data lr_node type ref to zif_ajson=>ty_node.
    data ls_new_node like line of mt_json_tree.
    data ls_split_path type zif_ajson=>ty_path_name.

    if mv_read_only = abap_true.
      zcx_ajson_error=>raise( 'This json instance is read only' ).
    endif.

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
      lr_node = get_item( iv_path ).
    endif.

    if lr_node is initial. " Or node was cleared

      data lr_parent type ref to zif_ajson=>ty_node.
      data lt_node_stack type tty_node_stack.

      lt_node_stack = prove_path_exists( ls_split_path-path ).
      read table lt_node_stack index 1 into lr_parent.
      assert sy-subrc = 0.
      lr_parent->children = lr_parent->children + 1.

      ls_new_node-path = ls_split_path-path.
      ls_new_node-name = ls_split_path-name.
      ls_new_node-type = zif_ajson=>node_type-array.
      insert ls_new_node into table mt_json_tree.

    elseif lr_node->type <> zif_ajson=>node_type-array.
      zcx_ajson_error=>raise( |Path [{ iv_path }] already used and is not array| ).
    endif.

  endmethod.


  method zif_ajson~freeze.
    mv_read_only = abap_true.
  endmethod.


  method zif_ajson~keep_item_order.
    mv_keep_item_order = abap_true.
  endmethod.
ENDCLASS.
