class ZCL_AJSON_FILTER_LIB definition
  public
  final
  create public .

  public section.

    class-methods create_empty_filter
      returning
        value(ri_filter) type ref to zif_ajson_filter
      raising
        zcx_ajson_error .
    class-methods create_path_filter
      importing
        !it_skip_paths type string_table
      returning
        value(ri_filter) type ref to zif_ajson_filter
      raising
        zcx_ajson_error .
  protected section.
  private section.
ENDCLASS.



CLASS ZCL_AJSON_FILTER_LIB IMPLEMENTATION.


  method create_empty_filter.
    create object ri_filter type lcl_empty_filter.
  endmethod.


  method create_path_filter.
    create object ri_filter type lcl_paths_filter
      exporting
        it_skip_paths = it_skip_paths.
  endmethod.
ENDCLASS.
