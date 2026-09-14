class lcl_rename implementation.

  method constructor.
    mt_rename_map = it_rename_map.
    mv_rename_by = iv_rename_by.
  endmethod.

  method zif_ajson_mapper~rename_node.

    data lv_full_path type string.
    data lv_pair_found type abap_bool.
    field-symbols <r> like line of mt_rename_map.

    case mv_rename_by.
      when zcl_ajson_mapper_lib=>rename_by-attr_name.
        read table mt_rename_map assigning <r> with table key by_name components from = cv_name.
        lv_pair_found = boolc( sy-subrc = 0 ).
      when zcl_ajson_mapper_lib=>rename_by-full_path.
        lv_full_path = is_node-path && cv_name.
        read table mt_rename_map assigning <r> with table key by_name components from = lv_full_path.
        lv_pair_found = boolc( sy-subrc = 0 ).
      when zcl_ajson_mapper_lib=>rename_by-pattern.
        lv_full_path = is_node-path && cv_name.
        loop at mt_rename_map assigning <r>.
          if lv_full_path cp <r>-from.
            lv_pair_found = abap_true.
            exit.
          endif.
        endloop.
      when others.
        lv_pair_found = abap_false. " No rename
    endcase.

    if lv_pair_found = abap_true.
      cv_name = <r>-to.
    endif.

  endmethod.

endclass.

class lcl_mapping_to_upper implementation.

  method zif_ajson_mapper~rename_node.
    cv_name = to_upper( cv_name ).
  endmethod.

endclass.


class lcl_mapping_to_lower implementation.

  method zif_ajson_mapper~rename_node.
    cv_name = to_lower( cv_name ).
  endmethod.

endclass.


class lcl_compound_mapper implementation.

  method constructor.
    mt_queue = it_queue.
  endmethod.

  method zif_ajson_mapper~rename_node.

    data ls_node like is_node.
    data li_mapper like line of mt_queue.

    ls_node = is_node.

    loop at mt_queue into li_mapper.
      li_mapper->rename_node(
        exporting
          is_node = ls_node
        changing
          cv_name = cv_name ).
      ls_node-name = cv_name.
    endloop.

  endmethod.

endclass.

class lcl_to_snake implementation.

  method zif_ajson_mapper~rename_node.

    replace all occurrences of regex `([a-z])([A-Z])` in cv_name with `$1_$2` ##REGEX_POSIX. "#EC NOTEXT
    cv_name = to_lower( cv_name ).

  endmethod.

endclass.

class lcl_to_camel implementation.

  method constructor.
    mv_first_json_upper = iv_first_json_upper.
  endmethod.

  method zif_ajson_mapper~rename_node.

    types lty_token type c length 255.
    constants lc_forced_underscore_marker type c length 1 value cl_abap_char_utilities=>horizontal_tab.

    data lt_tokens type standard table of lty_token.
    data lv_from type i.
    field-symbols <token> like line of lt_tokens.

    if mv_first_json_upper = abap_true.
      lv_from = 1.
    else.
      lv_from = 2.
    endif.
    replace all occurrences of `__` in cv_name with lc_forced_underscore_marker. " Force underscore

    split cv_name at `_` into table lt_tokens.
    delete lt_tokens where table_line is initial.
    loop at lt_tokens assigning <token> from lv_from.
      translate <token>+0(1) to upper case.
    endloop.

    concatenate lines of lt_tokens into cv_name.
    replace all occurrences of lc_forced_underscore_marker in cv_name with `_`.

  endmethod.

endclass.
