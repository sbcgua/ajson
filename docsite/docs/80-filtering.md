---
sidebar_position: 80
---

# Filtering

This feature allows creating a JSON from existing one skipping some nodes. E.g. empty values, predefined paths or using your custom filter.

## Predefined filters

- Remove empty values

```abap
  " li_json_source: { "a":1, "b":0, "c":{ "d":"" } }
  li_json_filtered = li_json_source->filter( zcl_ajson_filter_lib=>create_empty_filter( ) ).
  " li_json_filtered: { "a":1 }

  " OR ... (but prefer the former, this will be deprecated)
  li_json_filtered = zcl_ajson=>create_from(
    ii_source_json = li_json_source
    ii_filter = zcl_ajson_filter_lib=>create_empty_filter( ) ).
```

- Remove predefined paths

```abap
  " li_json_source: { "a":1, "b":0, "c":{ "d":"" } }
  li_json_filtered = li_json_source->filter( 
    zcl_ajson_filter_lib=>create_path_filter(
      it_skip_paths = value #( ( '/b' ) ( '/c' ) )
  ) ).
  " li_json_filtered: { "a":1 }
  
  " OR also
  ...
  zcl_ajson_filter_lib=>create_path_filter( iv_skip_paths = '/b,/c' ).
  ...
```

... works also with patterns (e.g. to remove meta data attrs)

```abap
  zcl_ajson_filter_lib=>create_path_filter( 
    iv_skip_paths = '*/@*'
    iv_pattern_search = abap_true ).
```

- compound ("and") filter

```abap
  ...
  zcl_ajson_filter_lib=>create_and_filter( value #(
    ( zcl_ajson_filter_lib=>create_empty_filter( ) )
    ( zcl_ajson_filter_lib=>create_path_filter( iv_skip_paths = '/xyz' ) )
  ) ).
  ...
```

## Custom filters

In order to apply a custom filter you have to implement a class with `zif_ajson_filter` interface. The interface has one method `keep_node` which receives `is_node` - JSON tree node of `zif_ajson=>ty_node` type and also the `iv_visit` param. `iv_visit` will be `zif_ajson_filter=>visit_type-value` for all final leafs (str,num,bool,null) and will get `visit_type-open` or `visit_type-close` values for objects and arrays. So the objects and arrays will be called twice - before and after filtering - this allows examining their children number before and after the current filtering. For example of implementation see local implementations of `zcl_ajson_filter_lib` class.

```abap
  method zif_ajson_filter~keep_node.
    " remove all nodes starting with 'x'
    rv_keep = boolc( is_node-name is initial or is_node-name+0(1) <> 'x' ).
  endmethod.
```
