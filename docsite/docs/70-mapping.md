---
sidebar_position: 70
---

# Mapping (field renaming)

## How it works

You can rename JSON attribute (node) names with a mapper. Typical example for this is making all attribute names upper/lower case or converting camel-snake naming styles (e.g. `helloWorld -> hello_world`).

```abap
  lo_orig_json = zcl_ajson=>parse( '{"ab":1,"bc":2}' ).
  lo_new_json = lo_orig_json->map( li_mapper ). -> " E.g. '{"AB":1,"BC":2}'

  " OR ... (but prefer the former, this one is deprected)
  lo_new_json = zcl_ajson=>create_from(
    ii_source_json = lo_orig_json
    ii_mapper      = li_mapper ). 
```

... where `li_mapper` would be an instance of `zif_ajson_mapping`.

Ajson implements a couple of frequent convertors in `zcl_ajson_mapping` class, in particular:

- upper/lower case
- to camel case (`camelCase`)
- to snake case (`snake_case`)

You can also implement you custom mapper. To do this you have to implement `zif_ajson_mapping->rename_node()`. It accepts the JSON nodes item-by-item and may change name via `cv_name` parameter. E.g.

```abap
  method zif_ajson_mapping~rename_field.
    if cv_name+0(1) = 'a'. " Upper case all fields that start with "a"
      cv_name = to_upper( cv_name ).
    endif.
  endmethod.
```

A realistic use case would be converting an external API result, which are often camel-cased (as this is very common in java-script world), and then converting it into ABAP structure:

```abap
  data:
    begin of ls_api_response,
      error_code type string,
      ...
    end of ls_api_response.

  lo_orig_json = zcl_ajson=>parse( lv_api_response_string ). " { "errorCode": 0, ... }
  lo_new_json = lo_orig_json->map( zcl_ajson_mapping=>camel_to_snake( ) ).
  lo_new_json->to_abap( importing ev_container = ls_api_response )
```

... or simpler and chained (combined with filter) ...

```abap
  zcl_ajson=>parse( lv_api_response_string
    )->filter( zcl_ajson_filter_lib=>create_path_filter(
      iv_skip_paths = '*/@*' " remove meta attributes
      iv_pattern_search = abap_true ) )
    )->map( zcl_ajson_mapping=>camel_to_snake( )
    )->to_abap( importing ev_container = ls_api_response ).
```

## "Boxed-in" mappers

Several typical mappers were implemented within `zcl_ajson_mapping` class:

- upper case node names

```abap
zcl_ajson=>parse( '{"a":1,"b":{"c":2}}'
  )->map( zcl_ajson_mapping=>create_upper_case( ) ).
  " {"A":1,"B":{"C":2}}
```

- lower case node names

```abap
zcl_ajson=>parse( '{"A":1,"B":{"C":2}}'
  )->map( zcl_ajson_mapping=>create_lower_case( ) ).
  " {"a":1,"b":{"c":2}}
```

- rename nodes

```abap
" Purely by name
zcl_ajson=>parse( '{"a":1,"b":{"c":2},"d":{"e":3}}'
  )->map( zcl_ajson_mapping=>create_rename( value #(
    ( from = 'a' to = 'x' )
    ( from = 'c' to = 'y' )
    ( from = 'd' to = 'z' ) )
  ) ).
  " {"b":{"y":2},"x":1,"z":{"e":3}}

" Or by full path
zcl_ajson=>parse( '{"a":1,"b":{"a":2},"c":{"a":3}}'
  )->map( zcl_ajson_mapping=>create_rename(
    it_rename_map = value #( ( from = '/b/a' to = 'x' ) )
    iv_rename_by  = zcl_ajson_mapping=>rename_by-full_path
  ) ).
  " {"a":1,"b":{"x":2},"c":{"a":3}}

" Or by pattern
zcl_ajson=>parse( '{"andthisnot":1,"b":{"thisone":2},"c":{"a":3}}'
  )->map( zcl_ajson_mapping=>create_rename(
    it_rename_map = value #( ( from = '/*/this*' to = 'x' ) )
    iv_rename_by  = zcl_ajson_mapping=>rename_by-pattern
  ) ).
  " {"andthisnot":1,"b":{"x":2},"c":{"a":3}}
```

- combine several arbitrary mappers together

```abap
zcl_ajson=>parse( '{"a":1,"b":{"a":2},"c":{"a":3}}'
  )->map( zcl_ajson_mapping=>create_compound_mapper(
    ii_mapper1 = zcl_ajson_mapping=>create_rename(
      it_rename_map = value #( ( from = '/b/a' to = 'x' ) )
      iv_rename_by  = zcl_ajson_mapping=>rename_by-full_path )
    ii_mapper2 = zcl_ajson_mapping=>create_upper_case( ) )
  ).
  " {"A":1,"B":{"X":2},"C":{"A":3}}'
```

- convert node names to snake case

```abap
zcl_ajson=>parse( '{"aB":1,"BbC":2,"cD":{"xY":3},"ZZ":4}'
  )->map( zcl_ajson_mapping=>create_to_snake_case( ) ).
  " {"a_b":1,"bb_c":2,"c_d":{"x_y":3},"zz":4}
```

- convert node names to camel case

```abap
zcl_ajson=>parse( '{"a_b":1,"bb_c":2,"c_d":{"x_y":3},"zz":4}'
  )->map( zcl_ajson_mapping=>create_to_camel_case( ) ).
  " {"aB":1,"bbC":2,"cD":{"xY":3},"zz":4}

" Optionally upper case first letter too
zcl_ajson=>parse( '{"aj_bc":1}'
  )->map( zcl_ajson_mapping=>create_to_camel_case(
    iv_first_json_upper = abap_true ) ).
  " {"AjBc":1}
```

All the above examples will also work with static `create_from()` method (but don't prefer it, might be deprecated).

```abap
zcl_ajson=>create_from(
  ii_source_json = zcl_ajson=>parse( '{"aj_bc":1}' )
  ii_mapper = zcl_ajson_mapping=>create_to_camel_case( )
).
  " {"ajBc":1}
```

### Mapping via to_abap and to_json (DEPRECATED)

**This approach is depreciated and will be removed in future versions, please use `rename_field` approach described above**

The interface `zif_ajson_mapping` allows to create custom mapping for ABAP and JSON fields via implementing `to_abap` and `to_json` methods.

Some mappings are provided by default:

- ABAP ⇆ JSON mapping fields
- JSON formatting to Camel Case
- JSON formatting to UPPER/lower case

#### Example: JSON => ABAP mapping fields

JSON Input

```json
{"field":"value","json.field":"field_value"}
```

Example code snippet

```abap
  data:
    lo_ajson          type ref to zcl_ajson,
    li_mapping        type ref to zif_ajson_mapping,
    lt_mapping_fields type zif_ajson_mapping=>ty_mapping_fields,
    ls_mapping_field  like line of lt_mapping_fields.
  data:
    begin of ls_result,
      abap_field type string,
      field      type string,
    end of ls_result.

  clear ls_mapping_field.
  ls_mapping_field-abap  = 'ABAP_FIELD'.
  ls_mapping_field-json = 'json.field'.
  insert ls_mapping_field into table lt_mapping_fields.

  li_mapping = zcl_ajson_mapping=>create_field_mapping( lt_mapping_fields ).

  lo_ajson =
      zcl_ajson=>parse( iv_json = '{"field":"value","json.field":"field_value"}' ii_custom_mapping = li_mapping ).

  lo_ajson->to_abap( importing ev_container = ls_result ).
```

#### Example: ABAP => JSON mapping fields

Example code snippet

```abap
  data:
    lo_ajson          type ref to zcl_ajson,
    li_mapping        type ref to zif_ajson_mapping,
    lt_mapping_fields type zif_ajson_mapping=>ty_mapping_fields,
    ls_mapping_field  like line of lt_mapping_fields.
  data:
    begin of ls_result,
      abap_field type string,
      field      type string,
    end of ls_result.

  clear ls_mapping_field.
  ls_mapping_field-abap  = 'ABAP_FIELD'.
  ls_mapping_field-json = 'json.field'.
  insert ls_mapping_field into table lt_mapping_fields.

  li_mapping = zcl_ajson_mapping=>create_field_mapping( lt_mapping_fields ).

  ls_result-abap_field = 'field_value'.
  ls_result-field      = 'value'.

  lo_ajson = zcl_ajson=>create_empty( ii_custom_mapping = li_mapping ).

  lo_ajson->set( iv_path = '/' iv_val = ls_result ).
```

JSON Output

```json
{"field":"value","json.field":"field_value"}
```

#### Example: Camel Case - To JSON (first letter lower case)

Example code snippet

```abap
  data:
    lo_ajson   type ref to zcl_ajson,
    li_mapping type ref to zif_ajson_mapping.
  data:
    begin of ls_result,
      field_data type string,
    end of ls_result.

  li_mapping = zcl_ajson_mapping=>create_camel_case( iv_first_json_upper = abap_false ).

  ls_result-field_data = 'field_value'.

  lo_ajson = zcl_ajson=>create_empty( ii_custom_mapping = li_mapping ).

  lo_ajson->set( iv_path = '/' iv_val = ls_result ).
```

JSON Output

```json
{"fieldData":"field_value"}
```

#### Example: Camel Case - To JSON (first letter upper case)

Example code snippet

```abap
  data:
    lo_ajson   type ref to zcl_ajson,
    li_mapping type ref to zif_ajson_mapping.
  data:
    begin of ls_result,
      field_data type string,
    end of ls_result.

  li_mapping = zcl_ajson_mapping=>create_camel_case( iv_first_json_upper = abap_true ).

  ls_result-field_data = 'field_value'.

  lo_ajson = zcl_ajson=>create_empty( ii_custom_mapping = li_mapping ).

  lo_ajson->set( iv_path = '/' iv_val = ls_result ).
```

JSON Output

```json
{"FieldData":"field_value"}
```

#### Example: Camel Case - To ABAP

JSON Input

```json
{"FieldData":"field_value"}
```

Example code snippet

```abap
  data:
    lo_ajson   type ref to zcl_ajson,
    li_mapping type ref to zif_ajson_mapping.
  data:
    begin of ls_result,
      field_data type string,
    end of ls_result.

  li_mapping = zcl_ajson_mapping=>create_camel_case( ).

  lo_ajson = zcl_ajson=>parse( iv_json = '{"FieldData":"field_value"}' ii_custom_mapping = li_mapping ).

  lo_ajson->to_abap( importing ev_container = ls_result ).
```

#### Example: Lower Case - To JSON

Example code snippet

```abap
  data:
    lo_ajson   type ref to zcl_ajson,
    li_mapping type ref to zif_ajson_mapping.
  data:
    begin of ls_result,
      field_data type string,
    end of ls_result.

  li_mapping = zcl_ajson_mapping=>create_lower_case( ).

  ls_result-field_data = 'field_value'.

  lo_ajson = zcl_ajson=>create_empty( ii_custom_mapping = li_mapping ).

  lo_ajson->set( iv_path = '/' iv_val = ls_result ).
```

JSON Output

```json
{"field_data":"field_value"}
```

#### Example: Upper Case - To JSON

Example code snippet

```abap
  data:
    lo_ajson   type ref to zcl_ajson,
    li_mapping type ref to zif_ajson_mapping.
  data:
    begin of ls_result,
      field_data type string,
    end of ls_result.

  li_mapping = zcl_ajson_mapping=>create_upper_case( ).

  ls_result-field_data = 'field_value'.

  lo_ajson = zcl_ajson=>create_empty( ii_custom_mapping = li_mapping ).

  lo_ajson->set( iv_path = '/' iv_val = ls_result ).
```

JSON Output

```json
{"FIELD_DATA":"field_value"}
```
