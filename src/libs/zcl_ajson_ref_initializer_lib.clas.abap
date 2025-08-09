class zcl_ajson_ref_initializer_lib definition
  public
  final
  create public.

  public section.

    class-methods create_path_refs_init
      importing
        !it_data_refs       type zif_ajson_ref_initializer=>tty_data_refs
      returning
        value(ri_refs_init) type ref to zif_ajson_ref_initializer
      raising
        zcx_ajson_error.

endclass.



class zcl_ajson_ref_initializer_lib implementation.


  method create_path_refs_init.
    create object ri_refs_init type lcl_path_refs_init
      exporting
        it_data_refs = it_data_refs.
  endmethod.
endclass.
