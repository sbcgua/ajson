class lcl_json_parser definition final.
  public section.

    methods parse
      importing
        iv_json type string
      returning
        value(rt_json_tree) type zcl_ajson=>ty_nodes_tt
      raising
        zcx_ajson_error.

  private section.

    types:
      ty_stack_tt type standard table of ref to zcl_ajson=>ty_node.

    data mt_stack type ty_stack_tt.

    class-methods join_path
      importing
        it_stack type ty_stack_tt
      returning
        value(rv_path) type string.

    methods raise
      importing
        iv_error type string
      raising
        zcx_ajson_error.

endclass.

class lcl_json_parser implementation.

  method parse.

    data lo_reader type ref to if_sxml_reader.
    data lr_stack_top like line of mt_stack.
    data lo_node type ref to if_sxml_node.
    field-symbols <item> like line of rt_json_tree.

    clear mt_stack.
    lo_reader = cl_sxml_string_reader=>create( cl_abap_codepage=>convert_to( iv_json ) ).

    " TODO: self protection, check non-empty, check starting from object ...

    do.
      lo_node = lo_reader->read_next_node( ).
      if lo_node is not bound.
        exit.
      endif.


      case lo_node->type.
        when if_sxml_node=>co_nt_element_open.
          data lt_attributes type if_sxml_attribute=>attributes.
          data lo_attr like line of lt_attributes.
          data lo_open type ref to if_sxml_open_element.
          lo_open ?= lo_node.

          append initial line to rt_json_tree assigning <item>.

          <item>-type = to_lower( lo_open->qname-name ).

          read table mt_stack index 1 into lr_stack_top.
          if sy-subrc = 0.
            <item>-path = join_path( mt_stack ).
            lr_stack_top->children = lr_stack_top->children + 1.

            if lr_stack_top->type = 'array'.
              <item>-name = |{ lr_stack_top->children }|.
              <item>-index = lr_stack_top->children.
            else.
              lt_attributes = lo_open->get_attributes( ).
              loop at lt_attributes into lo_attr.
                if lo_attr->qname-name = 'name' and lo_attr->value_type = if_sxml_value=>co_vt_text.
                  <item>-name = lo_attr->get_value( ).
                endif.
              endloop.
            endif.
          endif.

          get reference of <item> into lr_stack_top.
          insert lr_stack_top into mt_stack index 1.

        when if_sxml_node=>co_nt_element_close.
          data lo_close type ref to if_sxml_close_element.
          lo_close ?= lo_node.

          read table mt_stack index 1 into lr_stack_top.
          delete mt_stack index 1.
          if lo_close->qname-name <> lr_stack_top->type.
            raise( 'Unexpected closing node type' ).
          endif.

        when if_sxml_node=>co_nt_value.
          data lo_value type ref to if_sxml_value_node.
          lo_value ?= lo_node.

          <item>-value = lo_value->get_value( ).

        when others.
          raise( 'Unexpected node type' ).
      endcase.
    enddo.

    if lines( mt_stack ) > 0.
      raise( 'Unexpected end of data' ).
    endif.

  endmethod.

  method join_path.

    field-symbols <ref> like line of it_stack.

    loop at it_stack assigning <ref>.
      rv_path = <ref>->name && '/' && rv_path.
    endloop.

  endmethod.

  method raise.

    raise exception type zcx_ajson_error
      exporting
        location = join_path( mt_stack )
        rc       = 'PARS'
        message  = |JSON PARSER: { iv_error } @ { join_path( mt_stack ) }|.

  endmethod.

endclass.

**********************************************************************
* JSON_TO_ABAP
**********************************************************************

class lcl_json_to_abap definition final.
  public section.

    methods find_loc
      importing
        iv_path type string
        iv_name type string optional " not mandatory
        iv_append_tables type abap_bool default abap_false
      returning
        value(r_ref) type ref to data
      raising
        zcx_ajson_error.

    class-methods bind
      changing
        c_obj type any
        co_instance type ref to lcl_json_to_abap.

    methods to_abap
      importing
        it_nodes type zcl_ajson=>ty_nodes_ts
      raising
        zcx_ajson_error.

  private section.
    data mr_obj type ref to data.
endclass.

class lcl_json_to_abap implementation.

  method bind.
    create object co_instance.
    get reference of c_obj into co_instance->mr_obj.
  endmethod.

  method to_abap.

    data ref type ref to data.
    data lv_type type c.
    data lx type ref to cx_root.
    field-symbols <n> like line of it_nodes.
    field-symbols <value> type any.

    try.
      loop at it_nodes assigning <n> using key array_index.
        ref = find_loc(
          iv_append_tables = abap_true
          iv_path = <n>-path
          iv_name = <n>-name ).
        assign ref->* to <value>.
        assert sy-subrc = 0.
        describe field <value> type lv_type.

        case <n>-type.
          when 'null'.
            " Do nothing
          when 'bool'.
            <value> = boolc( <n>-value = 'true' ).
          when 'num'.
            <value> = <n>-value.
          when 'str'.
            <value> = <n>-value.
          when 'object'.
            if not lv_type co 'uv'.
              raise exception type zcx_ajson_error
                exporting
                  message  = 'Expected structure'
                  location = <n>-path && <n>-name.
            endif.
          when 'array'.
            if not lv_type co 'h'.
              raise exception type zcx_ajson_error
                exporting
                  message  = 'Expected table'
                  location = <n>-path && <n>-name.
            endif.
          when others.
            raise exception type zcx_ajson_error
              exporting
                message  = |Unexpected JSON type [{ <n>-type }]|
                location = <n>-path && <n>-name.
        endcase.

      endloop.
    catch cx_sy_conversion_no_number into lx.
      raise exception type zcx_ajson_error
        exporting
          message  = |Source is not a number|
          location = <n>-path && <n>-name.
    endtry.

  endmethod.

  method find_loc.

    data lt_path type string_table.
    data lv_trace type string.
    data lv_type type c.
    data lv_size type i.
    data lv_index type i.
    field-symbols <struc> type any.
    field-symbols <table> type standard table.
    field-symbols <value> type any.
    field-symbols <seg> like line of lt_path.

    split iv_path at '/' into table lt_path.
    delete lt_path where table_line is initial.
    if iv_name is not initial.
      append iv_name to lt_path.
    endif.

    r_ref = mr_obj.

    loop at lt_path assigning <seg>.
      lv_trace = lv_trace && '/' && <seg>.
      <seg> = to_upper( <seg> ).

      assign r_ref->* to <struc>.
      assert sy-subrc = 0.
      describe field <struc> type lv_type.

      if lv_type ca 'lr'. " data/obj ref
        " TODO maybe in future
        raise exception type zcx_ajson_error
          exporting
            message  = 'Cannot assign to ref'
            location = lv_trace.

      elseif lv_type = 'h'. " table
        if not <seg> co '0123456789'.
          raise exception type zcx_ajson_error
            exporting
              message  = 'Need index to access tables'
              location = lv_trace.
        endif.
        lv_index = <seg>.
        assign r_ref->* to <table>.
        assert sy-subrc = 0.

        lv_size = lines( <table> ).
        if iv_append_tables = abap_true and lv_index = lv_size + 1.
          append initial line to <table>.
        endif.

        read table <table> index lv_index assigning <value>.
        if sy-subrc <> 0.
          raise exception type zcx_ajson_error
            exporting
              message  = 'Index not found in table'
              location = lv_trace.
        endif.

      elseif lv_type ca 'uv'. " structure
        assign component <seg> of structure <struc> to <value>.
        if sy-subrc <> 0.
          raise exception type zcx_ajson_error
            exporting
              message  = 'Path not found'
              location = lv_trace.
        endif.
      else.
        raise exception type zcx_ajson_error
          exporting
            message  = 'Target is not deep'
            location = lv_trace.
      endif.
      get reference of <value> into r_ref.
    endloop.

  endmethod.

endclass.

**********************************************************************
* ABAP_TO_JSON
**********************************************************************

class lcl_abap_to_json definition final.
  public section.

    class-methods convert
      importing
        iv_data type any
        is_prefix type zcl_ajson=>ty_path_name optional
      returning
        value(rt_nodes) type zcl_ajson=>ty_nodes_tt
      raising
        zcx_ajson_error.

    class-methods class_constructor.

  private section.

    class-data gv_ajson_absolute_type_name type string.

    methods convert_any
      importing
        iv_data type any
        io_type type ref to cl_abap_typedescr
        is_prefix type zcl_ajson=>ty_path_name
        iv_index type i default 0
      changing
        ct_nodes type zcl_ajson=>ty_nodes_tt
      raising
        zcx_ajson_error.

    methods convert_ajson
      importing
        io_json type ref to zcl_ajson
        is_prefix type zcl_ajson=>ty_path_name
        iv_index type i default 0
      changing
        ct_nodes type zcl_ajson=>ty_nodes_tt.

    methods convert_value
      importing
        iv_data type any
        io_type type ref to cl_abap_typedescr
        is_prefix type zcl_ajson=>ty_path_name
        iv_index type i default 0
      changing
        ct_nodes type zcl_ajson=>ty_nodes_tt
      raising
        zcx_ajson_error.

    methods convert_struc
      importing
        iv_data type any
        io_type type ref to cl_abap_typedescr
        is_prefix type zcl_ajson=>ty_path_name
        iv_index type i default 0
      changing
        ct_nodes type zcl_ajson=>ty_nodes_tt
        cs_root  type zcl_ajson=>ty_node optional
      raising
        zcx_ajson_error.

    methods convert_table
      importing
        iv_data type any
        io_type type ref to cl_abap_typedescr
        is_prefix type zcl_ajson=>ty_path_name
        iv_index type i default 0
      changing
        ct_nodes type zcl_ajson=>ty_nodes_tt
      raising
        zcx_ajson_error.

endclass.

class zcl_ajson definition local friends lcl_abap_to_json.

class lcl_abap_to_json implementation.

  method class_constructor.

    data lo_dummy type ref to zcl_ajson.
    data lo_type type ref to cl_abap_refdescr.
    lo_type ?= cl_abap_typedescr=>describe_by_data( lo_dummy ).
    gv_ajson_absolute_type_name = lo_type->get_referenced_type( )->absolute_name.

  endmethod.

  method convert.

    data lo_type type ref to cl_abap_typedescr.
    data lo_converter type ref to lcl_abap_to_json.

    lo_type = cl_abap_typedescr=>describe_by_data( iv_data ).
    create object lo_converter.

    lo_converter->convert_any(
      exporting
        iv_data   = iv_data
        io_type   = lo_type
        is_prefix = is_prefix
      changing
        ct_nodes = rt_nodes ).

  endmethod.

  method convert_any.

    case io_type->kind.
      when cl_abap_typedescr=>kind_elem.
        convert_value(
          exporting
            iv_data   = iv_data
            io_type   = io_type
            is_prefix = is_prefix
            iv_index  = iv_index
          changing
            ct_nodes = ct_nodes ).

      when cl_abap_typedescr=>kind_struct.
        convert_struc(
          exporting
            iv_data   = iv_data
            io_type   = io_type
            is_prefix = is_prefix
            iv_index  = iv_index
          changing
            ct_nodes = ct_nodes ).

      when cl_abap_typedescr=>kind_table.
        convert_table(
          exporting
            iv_data   = iv_data
            io_type   = io_type
            is_prefix = is_prefix
            iv_index  = iv_index
          changing
            ct_nodes = ct_nodes ).

      when others.

        if io_type->type_kind = cl_abap_typedescr=>typekind_oref
          and cl_abap_typedescr=>describe_by_object_ref( iv_data )->absolute_name = gv_ajson_absolute_type_name.
          convert_ajson(
            exporting
              io_json   = iv_data
              is_prefix = is_prefix
              iv_index  = iv_index
            changing
              ct_nodes = ct_nodes ).
        else.
          raise exception type zcx_ajson_error
            exporting
              message = |Unsupported type [{ io_type->type_kind }] @{ is_prefix-path && is_prefix-name }|.
        endif.

    endcase.

  endmethod.

  method convert_ajson.

    field-symbols <n> like line of ct_nodes.

    ct_nodes = io_json->mt_json_tree.

    loop at ct_nodes assigning <n>.
      if <n>-path is initial and <n>-name is initial. " root node
        <n>-path  = is_prefix-path.
        <n>-name  = is_prefix-name.
        <n>-index = iv_index.
      else.
        <n>-path = is_prefix-path && is_prefix-name && <n>-path.
      endif.
    endloop.

  endmethod.

  method convert_value.

    field-symbols <n> like line of ct_nodes.

    append initial line to ct_nodes assigning <n>.

    <n>-path  = is_prefix-path.
    <n>-name  = is_prefix-name.
    <n>-index = iv_index.

    if io_type->absolute_name = '\TYPE-POOL=ABAP\TYPE=ABAP_BOOL' or io_type->absolute_name = '\TYPE=XFELD'.
      <n>-type = 'bool'.
      if iv_data is not initial.
        <n>-value = 'true'.
      else.
        <n>-value = 'false'.
      endif.
    elseif io_type->type_kind co 'CNgXyDT'. " Char like, date/time, xstring
      <n>-type = 'str'.
      <n>-value = |{ iv_data }|.
    elseif io_type->type_kind co 'bsI8PaeF'. " Numeric
      <n>-type = 'num'.
      <n>-value = |{ iv_data }|.
    else.
      raise exception type zcx_ajson_error
        exporting
          message = |Unexpected elemetary type [{ io_type->type_kind }] @{ is_prefix-path && is_prefix-name }|.
    endif.

  endmethod.

  method convert_struc.

    data lo_struc type ref to cl_abap_structdescr.
    data lt_comps type cl_abap_structdescr=>component_table.
    data ls_next_prefix like is_prefix.

    field-symbols <root> like line of ct_nodes.
    field-symbols <c> like line of lt_comps.
    field-symbols <val> type any.

    lo_struc ?= io_type.
    lt_comps = lo_struc->get_components( ).
    " get_components is potentially much slower than lo_struc->components
    " but ! we still need it to identify booleans
    " and rtti seems to cache type descriptions really well (https://github.com/sbcgua/benchmarks.git)
    " the structures will be repeated in real life

    if cs_root is supplied. " call for include structure
      assign cs_root to <root>.
    else. " First call
      append initial line to ct_nodes assigning <root>.
      <root>-path  = is_prefix-path.
      <root>-name  = is_prefix-name.
      <root>-type  = 'object'.
      <root>-index = iv_index.
    endif.

    ls_next_prefix-path = is_prefix-path && is_prefix-name && '/'.

    loop at lt_comps assigning <c>.

      if <c>-as_include = abap_true.

        convert_struc(
          exporting
            iv_data   = iv_data
            io_type   = <c>-type
            is_prefix = is_prefix
          changing
            cs_root  = <root>
            ct_nodes = ct_nodes ).

      else.

        <root>-children = <root>-children + 1.
        ls_next_prefix-name = to_lower( <c>-name ).
        assign component <c>-name of structure iv_data to <val>.
        assert sy-subrc = 0.

        convert_any(
          exporting
            iv_data   = <val>
            io_type   = <c>-type
            is_prefix = ls_next_prefix
          changing
            ct_nodes = ct_nodes ).

      endif.

    endloop.

  endmethod.

  method convert_table.

    data lo_table type ref to cl_abap_tabledescr.
    data lo_ltype type ref to cl_abap_typedescr.
    data ls_next_prefix like is_prefix.

    field-symbols <root> like line of ct_nodes.
    field-symbols <tab> type any table.
    field-symbols <val> type any.

    lo_table ?= io_type.
    lo_ltype = lo_table->get_table_line_type( ).

    append initial line to ct_nodes assigning <root>.
    <root>-path  = is_prefix-path.
    <root>-name  = is_prefix-name.
    <root>-type  = 'array'.
    <root>-index = iv_index.

    ls_next_prefix-path = is_prefix-path && is_prefix-name && '/'.
    assign iv_data to <tab>.

    loop at <tab> assigning <val>.
      ls_next_prefix-name = to_lower( |{ sy-tabix }| ).

      convert_any(
        exporting
          iv_data   = <val>
          io_type   = lo_ltype
          is_prefix = ls_next_prefix
          iv_index  = <root>-children + 1
        changing
          ct_nodes = ct_nodes ).

      <root>-children = <root>-children + 1.
    endloop.

  endmethod.

endclass.
