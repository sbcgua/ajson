class zcl_ajson_mapping definition
  public
  final
  create public.

  public section.
    class-methods create_camel_case
      importing
        it_mapping_fields   type zif_ajson_field_mapping=>ty_mapping_fields optional
        iv_first_json_upper type abap_bool default abap_true

      returning
        value(ri_mapping)   type ref to zif_ajson_field_mapping.

    class-methods create_upper_case
      importing
        it_mapping_fields type zif_ajson_field_mapping=>ty_mapping_fields optional
      returning
        value(ri_mapping) type ref to zif_ajson_field_mapping.

    class-methods create_lower_case
      importing
        it_mapping_fields type zif_ajson_field_mapping=>ty_mapping_fields optional
      returning
        value(ri_mapping) type ref to zif_ajson_field_mapping.

    class-methods create_field_mapping
      importing
        it_mapping_fields type zif_ajson_field_mapping=>ty_mapping_fields
      returning
        value(ri_mapping) type ref to zif_ajson_field_mapping.

  protected section.

  private section.

endclass.



class zcl_ajson_mapping implementation.


  method create_camel_case.

    create object ri_mapping type lcl_mapping_camel
      exporting
        it_mapping_fields   = it_mapping_fields
        iv_first_json_upper = iv_first_json_upper.

  endmethod.


  method create_upper_case.

    create object ri_mapping type lcl_mapping_to_upper
      exporting
        it_mapping_fields = it_mapping_fields.

  endmethod.


  method create_lower_case.

    create object ri_mapping type lcl_mapping_to_lower
      exporting
        it_mapping_fields = it_mapping_fields.

  endmethod.


  method create_field_mapping.

    create object ri_mapping type lcl_mapping_fields
      exporting
        it_mapping_fields = it_mapping_fields.

  endmethod.


endclass.
