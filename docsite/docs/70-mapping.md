---
sidebar_position: 70
---

# Mapping (field renaming)

## How it works

You can rename JSON attribute (node) names with a mapper. Typical example for this is making all attribute names upper/lower case or converting camel-snake naming styles (e.g. `helloWorld -> hello_world`).

```abap
  lo_orig_json = zcl_ajson=>parse( '{"ab":1,"bc":2}' ).
  lo_new_json = lo_orig_json->map( li_mapper ). -> " E.g. '{"AB":1,"BC":2}'
```

... where `li_mapper` would be an instance of `zif_ajson_mapper`.

Ajson implements a couple of frequent convertors in `zcl_ajson_mapper_lib` class, in particular:

- upper/lower case
- to camel case (`camelCase`)
- to snake case (`snake_case`)

You can also implement you custom mapper. To do this you have to implement `zif_ajson_mapper->rename_node()`. It accepts the JSON nodes item-by-item and may change name via `cv_name` parameter. E.g.

```abap
  method zif_ajson_mapper~rename_field.
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
  lo_new_json = lo_orig_json->map( zcl_ajson_mapper_lib=>camel_to_snake( ) ).
  lo_new_json->to_abap( importing ev_container = ls_api_response )
```

... or simpler and chained (combined with filter) ...

```abap
  zcl_ajson=>parse( lv_api_response_string
    )->filter( zcl_ajson_filter_lib=>create_path_filter(
      iv_skip_paths = '*/@*' " remove meta attributes
      iv_pattern_search = abap_true ) )
    )->map( zcl_ajson_mapper_lib=>camel_to_snake( )
    )->to_abap( importing ev_container = ls_api_response ).
```

## "Boxed-in" mappers

Several typical mappers were implemented within `zcl_ajson_mapper_lib` class:

- upper case node names

```abap
zcl_ajson=>parse( '{"a":1,"b":{"c":2}}'
  )->map( zcl_ajson_mapper_lib=>create_upper_case( ) ).
  " {"A":1,"B":{"C":2}}
```

- lower case node names

```abap
zcl_ajson=>parse( '{"A":1,"B":{"C":2}}'
  )->map( zcl_ajson_mapper_lib=>create_lower_case( ) ).
  " {"a":1,"b":{"c":2}}
```

- rename nodes

```abap
" Purely by name
zcl_ajson=>parse( '{"a":1,"b":{"c":2},"d":{"e":3}}'
  )->map( zcl_ajson_mapper_lib=>create_rename( value #(
    ( from = 'a' to = 'x' )
    ( from = 'c' to = 'y' )
    ( from = 'd' to = 'z' ) )
  ) ).
  " {"b":{"y":2},"x":1,"z":{"e":3}}

" Or by full path
zcl_ajson=>parse( '{"a":1,"b":{"a":2},"c":{"a":3}}'
  )->map( zcl_ajson_mapper_lib=>create_rename(
    it_rename_map = value #( ( from = '/b/a' to = 'x' ) )
    iv_rename_by  = zcl_ajson_mapper_lib=>rename_by-full_path
  ) ).
  " {"a":1,"b":{"x":2},"c":{"a":3}}

" Or by pattern
zcl_ajson=>parse( '{"andthisnot":1,"b":{"thisone":2},"c":{"a":3}}'
  )->map( zcl_ajson_mapper_lib=>create_rename(
    it_rename_map = value #( ( from = '/*/this*' to = 'x' ) )
    iv_rename_by  = zcl_ajson_mapper_lib=>rename_by-pattern
  ) ).
  " {"andthisnot":1,"b":{"x":2},"c":{"a":3}}
```

- combine several arbitrary mappers together

```abap
zcl_ajson=>parse( '{"a":1,"b":{"a":2},"c":{"a":3}}'
  )->map( zcl_ajson_mapper_lib=>create_compound_mapper(
    ii_mapper1 = zcl_ajson_mapper_lib=>create_rename(
      it_rename_map = value #( ( from = '/b/a' to = 'x' ) )
      iv_rename_by  = zcl_ajson_mapper_lib=>rename_by-full_path )
    ii_mapper2 = zcl_ajson_mapper_lib=>create_upper_case( ) )
  ).
  " {"A":1,"B":{"X":2},"C":{"A":3}}'
```

- convert node names to snake case

```abap
zcl_ajson=>parse( '{"aB":1,"BbC":2,"cD":{"xY":3},"ZZ":4}'
  )->map( zcl_ajson_mapper_lib=>create_to_snake_case( ) ).
  " {"a_b":1,"bb_c":2,"c_d":{"x_y":3},"zz":4}
```

- convert node names to camel case

```abap
zcl_ajson=>parse( '{"a_b":1,"bb_c":2,"c_d":{"x_y":3},"zz":4}'
  )->map( zcl_ajson_mapper_lib=>create_to_camel_case( ) ).
  " {"aB":1,"bbC":2,"cD":{"xY":3},"zz":4}

" Optionally upper case first letter too
zcl_ajson=>parse( '{"aj_bc":1}'
  )->map( zcl_ajson_mapper_lib=>create_to_camel_case(
    iv_first_json_upper = abap_true ) ).
  " {"AjBc":1}
```
