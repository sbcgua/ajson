**********************************************************************
* ITERATOR
**********************************************************************

class lcl_node_iterator definition final.
  public section.
    interfaces zif_ajson_iterator.
    methods constructor
      importing
        ii_json type ref to zif_ajson
        iv_path type string
        iv_node_type type zif_ajson_types=>ty_node_type
      raising
        zcx_ajson_error.

  private section.
    data mi_json type ref to zif_ajson.
    data mv_node_type type zif_ajson_types=>ty_node_type.
    data mv_base_path type string.
    data mr_cursor type ref to zif_ajson_types=>ty_node.
    data mv_tabix type i.
    data mv_has_next type abap_bool.
    methods find_first_node.

endclass.

class lcl_node_iterator implementation.

  method constructor.

    if not ( iv_node_type = zif_ajson_types=>node_type-array or iv_node_type = zif_ajson_types=>node_type-object ).
      zcx_ajson_error=>raise( |Iterator can iterate arrays or objects only ("{ iv_node_type }" passed)| ).
    endif.

    mv_base_path = zcl_ajson=>normalize_path( iv_path ).
    mv_node_type = iv_node_type.
    mi_json      = ii_json.

    data lv_node_type like mv_node_type.
    lv_node_type = ii_json->get_node_type( mv_base_path ).

    if lv_node_type is initial.
      zcx_ajson_error=>raise( |Path not found: { iv_path }| ).
    elseif mv_node_type = zif_ajson_types=>node_type-array and lv_node_type <> mv_node_type.
      zcx_ajson_error=>raise( |Array expected at: { iv_path }| ).
    elseif mv_node_type = zif_ajson_types=>node_type-object and lv_node_type <> mv_node_type.
      zcx_ajson_error=>raise( |Object expected at: { iv_path }| ).
    endif.

    find_first_node( ).

  endmethod.

  method find_first_node.

    case mv_node_type.
      when zif_ajson_types=>node_type-array.
        " path + array index key
        loop at mi_json->mt_json_tree reference into mr_cursor using key array_index where path = mv_base_path.
          mv_has_next = abap_true.
          mv_tabix    = sy-tabix.
          exit. " first found
        endloop.
      when zif_ajson_types=>node_type-object.
        " regular path + name key
        loop at mi_json->mt_json_tree reference into mr_cursor where path = mv_base_path.
          mv_has_next = abap_true.
          mv_tabix    = sy-tabix.
          exit. " first found
        endloop.
      when others.
        assert 1 = 0.
    endcase.

  endmethod.

  method zif_ajson_iterator~has_next.
    rv_yes = mv_has_next.
  endmethod.

  method zif_ajson_iterator~next.

    if mv_has_next = abap_false.
      return.
    endif.

    ri_item = mi_json->slice( |{ mr_cursor->path }{ mr_cursor->name }| ).
    " TODO: improve performance, see comment in slice, maybe reuse read only reference to node_tree

    mv_tabix = mv_tabix + 1.
    case mv_node_type.
      when zif_ajson_types=>node_type-array.
        " path + array index key
        read table mi_json->mt_json_tree
          index mv_tabix using key array_index
          reference into mr_cursor.
      when zif_ajson_types=>node_type-object.
        " regular path + name key
        read table mi_json->mt_json_tree
          index mv_tabix
          reference into mr_cursor.
      when others.
        assert 1 = 0.
    endcase.
    mv_has_next = boolc( sy-subrc = 0 and mr_cursor->path = mv_base_path ).

  endmethod.

endclass.
