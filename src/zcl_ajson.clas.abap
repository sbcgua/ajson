class zcl_ajson definition
  public
  create private .

  public section.

    interfaces zif_ajson .

    aliases:
      is_empty for zif_ajson~is_empty,
      exists for zif_ajson~exists,
      members for zif_ajson~members,
      get for zif_ajson~get,
      get_boolean for zif_ajson~get_boolean,
      get_integer for zif_ajson~get_integer,
      get_number for zif_ajson~get_number,
      get_date for zif_ajson~get_date,
      get_timestamp for zif_ajson~get_timestamp,
      get_string for zif_ajson~get_string,
      slice for zif_ajson~slice,
      to_abap for zif_ajson~to_abap,
      array_to_string_table for zif_ajson~array_to_string_table.

    aliases:
      clear for zif_ajson~clear,
      set for zif_ajson~set,
      set_boolean for zif_ajson~set_boolean,
      set_string for zif_ajson~set_string,
      set_integer for zif_ajson~set_integer,
      set_date for zif_ajson~set_date,
      set_timestamp for zif_ajson~set_timestamp,
      set_null for zif_ajson~set_null,
      delete for zif_ajson~delete,
      touch_array for zif_ajson~touch_array,
      push for zif_ajson~push,
      stringify for zif_ajson~stringify.

    aliases:
      mt_json_tree for zif_ajson~mt_json_tree,
      keep_item_order for zif_ajson~keep_item_order,
      format_datetime for zif_ajson~format_datetime,
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

    " Experimental ! May change
    class-methods create_from " TODO, rename to 'from' ?
      importing
        !ii_source_json type ref to zif_ajson
        !ii_filter type ref to zif_ajson_filter optional
        !ii_mapper type ref to zif_ajson_mapping optional
      returning
        value(ro_instance) type ref to zcl_ajson
      raising
        zcx_ajson_error .

    methods constructor.

  protected section.

  private section.

    data mv_read_only type abap_bool.
    data mi_custom_mapping type ref to zif_ajson_mapping.
    data mv_keep_item_order type abap_bool.
    data mv_format_datetime type abap_bool.
    " TODO restructure into zif_ajson=>ty_opts

    methods get_item
      importing
        iv_path        type string
      returning
        value(rv_item) type ref to zif_ajson=>ty_node.
    methods prove_path_exists
      importing
        iv_path              type string
      returning
        value(rr_end_node) type ref to zif_ajson=>ty_node
      raising
        zcx_ajson_error.
    methods delete_subtree
      importing
        iv_path           type string
        iv_name           type string
        ir_parent         type ref to zif_ajson=>ty_node optional
      returning
        value(rs_top_node) type zif_ajson=>ty_node.
    methods read_only_watchdog
      raising
        zcx_ajson_error.
ENDCLASS.



CLASS ZCL_AJSON IMPLEMENTATION.

  method zif_ajson~opts.
    rs_opts-read_only       = mv_read_only.
    rs_opts-format_datetime = mv_format_datetime.
    rs_opts-keep_item_order = mv_keep_item_order.
  endmethod.

  method constructor.
    format_datetime( abap_true ).
  endmethod.


  method create_empty.
    create object ro_instance.
    ro_instance->mi_custom_mapping = ii_custom_mapping.
  endmethod.


  method create_from.

    data lt_buf like ii_source_json->mt_json_tree.
    field-symbols <from> like ii_source_json->mt_json_tree.
    field-symbols <to> like ii_source_json->mt_json_tree.

    if ii_source_json is not bound.
      zcx_ajson_error=>raise( 'Source not bound' ).
    endif.

    create object ro_instance.

    if ii_filter is not bound and ii_mapper is not bound.
      ro_instance->mt_json_tree = ii_source_json->mt_json_tree.
    else.
      assign ii_source_json->mt_json_tree to <from>.

      " Mapping goes first. But maybe it should be a freely definable queue of processors ?
      if ii_mapper is bound.
        if ii_filter is bound.
          assign lt_buf to <to>.
        else.
          assign ro_instance->mt_json_tree to <to>.
        endif.

        lcl_mapper_runner=>new( ii_mapper )->lif_mutator_runner~run(
          exporting
            it_source_tree = <from>
          importing
            et_dest_tree = <to> ).

        assign lt_buf to <from>.
      endif.

      assign ro_instance->mt_json_tree to <to>.

      if ii_filter is bound.
        lcl_filter_runner=>new( ii_filter )->lif_mutator_runner~run(
          exporting
            it_source_tree = <from>
          importing
            et_dest_tree = <to> ).
      endif.
    endif.

    ro_instance->mv_format_datetime = ii_source_json->opts( )-format_datetime.
    ro_instance->mv_keep_item_order = ii_source_json->opts( )-keep_item_order.

  endmethod.


  method delete_subtree.

    data lv_parent_path type string.
    data lr_parent like ir_parent.

    read table mt_json_tree into rs_top_node
      with key
        path = iv_path
        name = iv_name.
    if sy-subrc <> 0.
      return. " Not found ? nothing to delete !
    endif.

    delete mt_json_tree index sy-tabix. " where path = iv_path and name = iv_name.

    if rs_top_node-children > 0. " only for objects and arrays
      lv_parent_path = iv_path && iv_name && '/*'.
      delete mt_json_tree where path cp lv_parent_path.
    endif.

    " decrement parent children
    if ir_parent is supplied.
      ir_parent->children = ir_parent->children - 1.
    else.
      lr_parent = get_item( iv_path ).
      if lr_parent is not initial.
        lr_parent->children = lr_parent->children - 1.
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
    data lr_node_parent like rr_end_node.
    data lv_cur_path type string.
    data lv_cur_name type string.
    data ls_new_node like line of mt_json_tree.

    split iv_path at '/' into table lt_path.
    delete lt_path where table_line is initial.

    do.
      lr_node_parent = rr_end_node.
      read table mt_json_tree reference into rr_end_node
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
        insert ls_new_node into table mt_json_tree reference into rr_end_node.
      endif.
      lv_cur_path = lv_cur_path && lv_cur_name && '/'.
      read table lt_path index sy-index into lv_cur_name.
      if sy-subrc <> 0.
        exit. " no more segments
      endif.
    enddo.

  endmethod.


  method read_only_watchdog.
    if mv_read_only = abap_true.
      zcx_ajson_error=>raise( 'This json instance is read only' ).
    endif.
  endmethod.


  method zif_ajson~array_to_string_table.

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


  method zif_ajson~clear.

    read_only_watchdog( ).
    clear mt_json_tree.

  endmethod.


  method zif_ajson~delete.

    read_only_watchdog( ).

    data ls_split_path type zif_ajson=>ty_path_name.
    ls_split_path = lcl_utils=>split_path( iv_path ).

    delete_subtree(
      iv_path = ls_split_path-path
      iv_name = ls_split_path-name ).

    ri_json = me.

  endmethod.


  method zif_ajson~exists.
    rv_exists = boolc( get_item( iv_path ) is not initial ).
  endmethod.


  method zif_ajson~format_datetime.
    mv_format_datetime = iv_use_iso.
    ri_json = me.
  endmethod.


  method zif_ajson~freeze.
    mv_read_only = abap_true.
  endmethod.


  method zif_ajson~get.

    data lr_item type ref to zif_ajson=>ty_node.
    lr_item = get_item( iv_path ).
    if lr_item is not initial.
      rv_value = lr_item->value.
    endif.

  endmethod.


  method zif_ajson~get_boolean.

    data lr_item type ref to zif_ajson=>ty_node.
    lr_item = get_item( iv_path ).
    if lr_item is initial or lr_item->type = zif_ajson=>node_type-null.
      return.
    elseif lr_item->type = zif_ajson=>node_type-boolean.
      rv_value = boolc( lr_item->value = 'true' ).
    elseif lr_item->value is not initial.
      rv_value = abap_true.
    endif.

  endmethod.


  method zif_ajson~get_date.

    data lr_item type ref to zif_ajson=>ty_node.
    data lv_y type c length 4.
    data lv_m type c length 2.
    data lv_d type c length 2.

    lr_item = get_item( iv_path ).

    if lr_item is not initial and lr_item->type = zif_ajson=>node_type-string.
      find first occurrence of regex '^(\d{4})-(\d{2})-(\d{2})(T|$)' "#EC NOTEXT
        in lr_item->value
        submatches lv_y lv_m lv_d.
      concatenate lv_y lv_m lv_d into rv_value.
    endif.

  endmethod.


  method zif_ajson~get_integer.

    data lr_item type ref to zif_ajson=>ty_node.
    lr_item = get_item( iv_path ).
    if lr_item is not initial and lr_item->type = zif_ajson=>node_type-number.
      rv_value = lr_item->value.
    endif.

  endmethod.


  method zif_ajson~get_node_type.

    data lr_item type ref to zif_ajson=>ty_node.
    lr_item = get_item( iv_path ).
    if lr_item is not initial.
      rv_node_type = lr_item->type.
    endif.

  endmethod.


  method zif_ajson~get_number.

    data lr_item type ref to zif_ajson=>ty_node.
    lr_item = get_item( iv_path ).
    if lr_item is not initial and lr_item->type = zif_ajson=>node_type-number.
      rv_value = lr_item->value.
    endif.

  endmethod.


  method zif_ajson~get_string.

    data lr_item type ref to zif_ajson=>ty_node.
    lr_item = get_item( iv_path ).
    if lr_item is not initial and lr_item->type <> zif_ajson=>node_type-null.
      rv_value = lr_item->value.
    endif.

  endmethod.


  method zif_ajson~get_timestamp.

    data lo_to_abap type ref to lcl_json_to_abap.
    data lr_item type ref to zif_ajson=>ty_node.

    lr_item = get_item( iv_path ).

    if lr_item is initial.
      return.
    endif.

    create object lo_to_abap.

    try.
      rv_value = lo_to_abap->to_timestamp( lr_item->value ).
    catch zcx_ajson_error.
      return.
    endtry.

  endmethod.


  method zif_ajson~is_empty.
    rv_yes = boolc( lines( mt_json_tree ) = 0 ).
  endmethod.


  method zif_ajson~keep_item_order.
    mv_keep_item_order = abap_true.
    ri_json = me.
  endmethod.


  method zif_ajson~members.

    data lv_normalized_path type string.
    field-symbols <item> like line of mt_json_tree.

    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).

    loop at mt_json_tree assigning <item> where path = lv_normalized_path.
      append <item>-name to rt_members.
    endloop.

  endmethod.


  method zif_ajson~push.

    data lr_parent type ref to zif_ajson=>ty_node.
    data lr_new_node type ref to zif_ajson=>ty_node.

    read_only_watchdog( ).

    lr_parent = get_item( iv_path ).

    if lr_parent is initial.
      zcx_ajson_error=>raise( |Path [{ iv_path }] does not exist| ).
    endif.

    if lr_parent->type <> zif_ajson=>node_type-array.
      zcx_ajson_error=>raise( |Path [{ iv_path }] is not array| ).
    endif.

    data lt_new_nodes type zif_ajson=>ty_nodes_tt.
    data ls_new_path type zif_ajson=>ty_path_name.
    data lv_new_index type i.

    lv_new_index     = lr_parent->children + 1.
    ls_new_path-path = lcl_utils=>normalize_path( iv_path ).
    ls_new_path-name = |{ lv_new_index }|.

    lt_new_nodes = lcl_abap_to_json=>convert(
      iv_keep_item_order = mv_keep_item_order
      iv_data   = iv_val
      is_prefix = ls_new_path ).
    read table lt_new_nodes index 1 reference into lr_new_node. " assume first record is the array item - not ideal !
    assert sy-subrc = 0.
    lr_new_node->index = lv_new_index.

    " update data
    lr_parent->children = lv_new_index.
    insert lines of lt_new_nodes into table mt_json_tree.

    ri_json = me.

  endmethod.


  method zif_ajson~set.

    data ls_split_path type zif_ajson=>ty_path_name.
    data lr_parent type ref to zif_ajson=>ty_node.
    data ls_deleted_node type zif_ajson=>ty_node.

    read_only_watchdog( ).

    ri_json = me.

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
          iv_format_datetime = mv_format_datetime
          iv_keep_item_order = mv_keep_item_order
          iv_data            = iv_val
          iv_type            = iv_node_type
          is_prefix          = ls_split_path
          ii_custom_mapping  = mi_custom_mapping ).
      else.
        mt_json_tree = lcl_abap_to_json=>convert(
          iv_format_datetime = mv_format_datetime
          iv_keep_item_order = mv_keep_item_order
          iv_data            = iv_val
          is_prefix          = ls_split_path
          ii_custom_mapping  = mi_custom_mapping ).
      endif.
      return.
    endif.

    " Ensure whole path exists
    lr_parent = prove_path_exists( ls_split_path-path ).
    assert lr_parent is not initial.

    " delete if exists with subtree
    ls_deleted_node = delete_subtree(
      ir_parent = lr_parent
      iv_path   = ls_split_path-path
      iv_name   = ls_split_path-name ).

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
        iv_format_datetime = mv_format_datetime
        iv_keep_item_order = mv_keep_item_order
        iv_item_order      = ls_deleted_node-order
        iv_data            = iv_val
        iv_type            = iv_node_type
        iv_array_index     = lv_array_index
        is_prefix          = ls_split_path
        ii_custom_mapping  = mi_custom_mapping ).
    else.
      lt_new_nodes = lcl_abap_to_json=>convert(
        iv_format_datetime = mv_format_datetime
        iv_keep_item_order = mv_keep_item_order
        iv_item_order      = ls_deleted_node-order
        iv_data            = iv_val
        iv_array_index     = lv_array_index
        is_prefix          = ls_split_path
        ii_custom_mapping  = mi_custom_mapping ).
    endif.

    " update nodes
    if lines( lt_new_nodes ) > 0.
      lr_parent->children = lr_parent->children + 1.
      insert lines of lt_new_nodes into table mt_json_tree.
    endif.

  endmethod.


  method zif_ajson~set_boolean.

    ri_json = me.

    data lv_bool type abap_bool.
    lv_bool = boolc( iv_val is not initial ).
    zif_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_bool ).

  endmethod.


  method zif_ajson~set_date.

    ri_json = me.

    data lv_val type string.
    lv_val = lcl_abap_to_json=>format_date( iv_val ).

    zif_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_val ).

  endmethod.


  method zif_ajson~set_integer.

    ri_json = me.

    zif_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = iv_val ).

  endmethod.


  method zif_ajson~set_null.

    ri_json = me.

    data lv_null_ref type ref to data.
    zif_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_null_ref ).

  endmethod.


  method zif_ajson~set_string.

    ri_json = me.

    data lv_val type string.
    lv_val = iv_val.
    zif_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_val ).

  endmethod.


  method zif_ajson~set_timestamp.

    ri_json = me.

    data lv_timestamp_iso type string.
    lv_timestamp_iso = lcl_abap_to_json=>format_timestamp( iv_val ).

    zif_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_timestamp_iso ).

  endmethod.


  method zif_ajson~slice.

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


  method zif_ajson~stringify.

    rv_json = lcl_json_serializer=>stringify(
      it_json_tree       = mt_json_tree
      iv_keep_item_order = mv_keep_item_order
      iv_indent          = iv_indent ).

  endmethod.


  method zif_ajson~touch_array.

    data lr_node type ref to zif_ajson=>ty_node.
    data ls_deleted_node type zif_ajson=>ty_node.
    data ls_new_node like line of mt_json_tree.
    data ls_split_path type zif_ajson=>ty_path_name.

    read_only_watchdog( ).

    ls_split_path = lcl_utils=>split_path( iv_path ).
    if ls_split_path is initial. " Assign root, exceptional processing
      ls_new_node-path = ls_split_path-path.
      ls_new_node-name = ls_split_path-name.
      ls_new_node-type = 'array'.
      insert ls_new_node into table mt_json_tree.
      return.
    endif.

    if iv_clear = abap_true.
      ls_deleted_node = delete_subtree(
        iv_path = ls_split_path-path
        iv_name = ls_split_path-name ).
    else.
      lr_node = get_item( iv_path ).
    endif.

    if lr_node is initial. " Or node was cleared

      data lr_parent type ref to zif_ajson=>ty_node.
      lr_parent = prove_path_exists( ls_split_path-path ).
      assert lr_parent is not initial.

      lr_parent->children = lr_parent->children + 1.

      ls_new_node-path = ls_split_path-path.
      ls_new_node-name = ls_split_path-name.
      ls_new_node-type = zif_ajson=>node_type-array.

      if mv_keep_item_order = abap_true and ls_deleted_node is not initial.
        ls_new_node-order = ls_deleted_node-order.
      endif.

      insert ls_new_node into table mt_json_tree.

    elseif lr_node->type <> zif_ajson=>node_type-array.
      zcx_ajson_error=>raise( |Path [{ iv_path }] already used and is not array| ).
    endif.

    ri_json = me.

  endmethod.


  method zif_ajson~to_abap.

    data lo_to_abap type ref to lcl_json_to_abap.

    clear ev_container.
    create object lo_to_abap
      exporting
        ii_custom_mapping = mi_custom_mapping.

    lo_to_abap->to_abap(
      exporting
        it_nodes    = zif_ajson~mt_json_tree
      changing
        c_container = ev_container ).

  endmethod.

  method zif_ajson~clone.
    ri_json = zcl_ajson=>create_from( me ).
  endmethod.

  method zif_ajson~filter.
    ri_json = zcl_ajson=>create_from(
      ii_source_json = me
      ii_filter      = ii_filter ).
  endmethod.

  method zif_ajson~map.
    ri_json = zcl_ajson=>create_from(
      ii_source_json = me
      ii_mapper      = ii_mapper ).
  endmethod.

ENDCLASS.
