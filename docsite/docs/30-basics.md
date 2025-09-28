---
sidebar_position: 30
---

# Basics

- To **parse** existing JSON data - call `zcl_ajson=>parse( lv_json_string )`
- To create a new **empty** JSON instance (to set values and serialize) - call `zcl_ajson=>new( )` or `new zcl_ajson( )` in newer ABAP syntax
- You can **clone** an ajson instance to get a new independent copy with `lo_orig_json->clone( )`

## General rules

- All functional methods and types are defined in `zif_ajson` interface.
  - Methods have aliases in the `zcl_ajson` class, however please restrain from using them directly as they may be *deprecated* in future.
- Json attributes are addressed by path in form `/obj1/obj2/value` of e.g. `->get( '/a/b/c' )` addresses `{ "a": { "b": { "c": "this value !" } } }`
- Array items addressed with index starting from 1: `/tab/2/val` -> `{ "tab": [ {...}, { "val": "this value !" } ] }`

## Support for '/' in attribute names

*TBD*

## Chaining

- `Set` (and some other) methods also return `me` to support chaining: `li_json->set(...)->set(...)->touch_array(...)->push(...)`.

## Mapping and filtering libraries

- Mapping and formatting are enabled by interface `zif_ajson_mapping`. Predefined patterns for field mapping (ABAP ⇆ JSON), Camel Case, UPPER/lower case can be found in class `zcl_ajson_mapping`
- Predefined filters are available in `zcl_ajson_filter_lib` class

## Creating from (deprecated?)

```abap
  lo_new_json = zcl_ajson=>create_from(
    ii_source_json = lo_orig_json ).
```
