class zcl_ajson_refs definition
  public
  final
  create public.

  public section.
    interfaces zif_ajson_refs.

    methods constructor
      importing
        it_data_refs type zif_ajson_refs~tty_data_refs.

  private section.
    data mt_data_refs type zif_ajson_refs~tty_data_refs.
endclass.



class zcl_ajson_refs implementation.


  method constructor.
    mt_data_refs = it_data_refs.
  endmethod.


  method zif_ajson_refs~get_data_ref.

    field-symbols <data_ref> like line of mt_data_refs.

    read table mt_data_refs assigning <data_ref>
      with key by_path components path = is_node-path name = is_node-name.
    if sy-subrc = 0.
      ro_ref = <data_ref>-dref.
    endif.

  endmethod.
endclass.
