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
ENDCLASS.



CLASS zcl_ajson_refs IMPLEMENTATION.


  method constructor.
    mt_data_refs = it_data_refs.
  endmethod.


  method zif_ajson_refs~get_data_ref.

    field-symbols <ls_data_refs> like line of mt_data_refs.

    read table mt_data_refs assigning <ls_data_refs>
      with key by_path components path = is_node-path name = is_node-name.
    if sy-subrc = 0.
      ro_ref = <ls_data_refs>-dref.
    endif.

  endmethod.
ENDCLASS.
