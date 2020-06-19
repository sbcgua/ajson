class zcl_ajson definition
  public
  create private .

  public section.

    interfaces zif_ajson_reader .

    types:
      begin of ty_node,
        path type string,
        name type string,
        type type string,
        value type string,
        children type i,
      end of ty_node .
    types:
      ty_nodes_tt type standard table of ty_node with key path name .
    types:
      ty_nodes_ts type sorted table of ty_node with unique key path name .

    class-methods parse
      importing
        !iv_json type string
      returning
        value(ro_instance) type ref to zcl_ajson
      raising
        zcx_ajson_error .
  protected section.

  private section.

    types:
      begin of ty_path_name,
        path type string,
        name type string,
      end of ty_path_name.

    data mt_json_tree type ty_nodes_ts.

    class-methods normalize_path
      importing
        iv_path type string
      returning
        value(rv_path) type string.
    class-methods split_path
      importing
        iv_path type string
      returning
        value(rv_path_name) type ty_path_name.
    methods get_item
      importing
        iv_path type string
      returning
        value(rv_item) type ref to ty_node.

ENDCLASS.



CLASS ZCL_AJSON IMPLEMENTATION.


  method get_item.

    field-symbols <item> like line of mt_json_tree.
    data ls_path_name type ty_path_name.
    ls_path_name = split_path( iv_path ).

    read table mt_json_tree
      assigning <item>
      with key
        path = ls_path_name-path
        name = ls_path_name-name.
    if sy-subrc = 0.
      get reference of <item> into rv_item.
    endif.

  endmethod.


  method normalize_path.

    rv_path = iv_path.
    if strlen( rv_path ) = 0.
      rv_path = '/'.
    endif.
    if rv_path+0(1) <> '/'.
      rv_path = '/' && rv_path.
    endif.
    if substring( val = rv_path off = strlen( rv_path ) - 1 ) <> '/'.
      rv_path = rv_path && '/'.
    endif.

  endmethod.


  method parse.

    data lo_parser type ref to lcl_json_parser.

    create object ro_instance.
    create object lo_parser.
    ro_instance->mt_json_tree = lo_parser->parse( iv_json ).

  endmethod.


  method split_path.

    data lv_offs type i.
    data lv_len type i.
    data lv_trim_slash type i.

    lv_len = strlen( iv_path ).
    if lv_len = 0 or iv_path = '/'.
      return. " empty path is the alias for root item = '' + ''
    endif.

    if substring( val = iv_path off = lv_len - 1 ) = '/'.
      lv_trim_slash = 1. " ignore last '/'
    endif.

    lv_offs = find( val = reverse( iv_path ) sub = '/' off = lv_trim_slash ).
    if lv_offs = -1.
      lv_offs  = lv_len. " treat whole string as the 'name' part
    endif.
    lv_offs = lv_len - lv_offs.

    rv_path_name-path = normalize_path( substring( val = iv_path len = lv_offs ) ).
    rv_path_name-name = substring( val = iv_path off = lv_offs len = lv_len - lv_offs - lv_trim_slash ).

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

    lv_normalized_path = normalize_path( iv_path ).

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
    lv_normalized_path = normalize_path( iv_path ).
    lv_path_len        = strlen( lv_normalized_path ).
    ls_path_parts      = split_path( lv_normalized_path ).

    loop at mt_json_tree into ls_item.
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
    lo_to_abap = lcl_json_to_abap=>bind( changing c_obj = ev_container ).
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
ENDCLASS.
