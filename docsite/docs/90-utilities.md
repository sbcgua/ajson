---
sidebar_position: 90
---

# Utilities

Class `zcl_ajson_utilities` provides the following methods:

- `new` - static method to create an instance (the shortcut for pre 7.4 ABAP releases)
- `diff` - returns all inserts, deletions, and changes between two JSON objects
- `sort` - returns JSON string with nodes sorted alphabetically
- `is_equal` - returns true if 2 JSONs (or JSON string) are deeply equal
- `merge` - merges 2 JSONs together
- `iterate_array` - iterates through nodes of an array
- `iterate_object` - iterates through nodes of an object

## Difference between JSON

The delta between two JSON objects or strings is returned as three JSON objects containing nodes that where inserted, deleted, or changed.

Notes:

- In case the type of a node changes, it is returned as a deletion of the old node and an insert of the new node (since arrays or objects could be involved).
- The order of nodes is irrelevant for the comparison.

```abap
  data:
    lo_util       type ref to zcl_ajson_utilities,
    lv_original   type string,
    lv_comparison type string,
    lo_insert     type ref to zcl_ajson,
    lo_delete     type ref to zcl_ajson,
    lo_change     type ref to zcl_ajson.

  lv_original = '{"a":1,"b":"B","e":[],"f":[5]}'.
  
  lv_comparison = '{"a":2,"c":"C","e":[1],"f":[4]}'.
  
  create object lo_util.

  lo_util->diff(
    exporting
      iv_json_a = lv_original
      iv_json_b = lv_comparison
    importing
      eo_insert = lo_insert
      eo_delete = lo_delete
      eo_change = lo_change ).

  " lo_insert
  " {"c":"C","e":[1]}
  " lo_delete
  " {"b":"B"}
  " lo_change
  " {"a":2,"f":[5]}
```

You can see a more complex example in the test class of `zcl_ajson_utilities`.

## Sorting of JSON object or string

```abap
  data:
    lo_util type ref to zcl_ajson_utilities,
    lv_original type string,
    lv_sorted type string.
    
  lv_original = '{"e":[],"b":"B","f":[5],"a":1}'.

  create object lo_util.

  lv_sorted = lo_util->sort( iv_json = lv_original ).
  " {
  "   "a": 1,
  "   "b": "B",
  "   "e": [],
  "   "f": [
  "     5
  "   ]
  " }
```

## Testing equality

```abap
  zcl_ajson_utilities=>new( )->is_equal(
    iv_json_a = '{"a":1,"b":2}'
    iv_json_b = '{"a":1,"b":2}' ).       " Return abap_true
  zcl_ajson_utilities=>new( )->is_equal(
    iv_json_a = '{"a":1,"b":2}'
    iv_json_b = '{"a":1,"b":2,"c":3}' ). " Return abap_false
```

## Iterating through arrays and objects

Use `iterate_array()` or `iterate_object()` to visit the immediate children of a JSON node. Both methods take a JSON instance (`ii_json`) and the path to the container (`iv_path`), and return a `zif_ajson_iterator`. Use `/` to iterate through a root array or object.

Call `has_next()` before `next()`. Each call to `next()` returns the next child as a separate JSON slice, with that child at its root. Paths used on the returned item are relative to this new root.

### Arrays

Array items are visited in numeric index order (1, 2, 3, ...), including when an array has more than nine items. This example reads a field from each object in an array:

```abap
  data li_json type ref to zif_ajson.
  data li_iterator type ref to zif_ajson_iterator.
  data li_item type ref to zif_ajson.
  data lt_names type string_table.

  li_json = zcl_ajson=>parse(
    '{"users":[{"name":"Ada"},{"name":"Grace"}]}' ).
  li_iterator = zcl_ajson_utilities=>iterate_array(
    ii_json = li_json
    iv_path = '/users' ).

  while li_iterator->has_next( ) = abap_true.
    li_item = li_iterator->next( ).
    append li_item->get( '/name' ) to lt_names.
  endwhile.

  " lt_names contains Ada, Grace
```

For an array of scalar values, read each returned item at `/`:

```abap
  data li_json type ref to zif_ajson.
  data li_iterator type ref to zif_ajson_iterator.
  data li_item type ref to zif_ajson.
  data lt_values type string_table.

  li_json = zcl_ajson=>parse( '[10,20,30]' ).
  li_iterator = zcl_ajson_utilities=>iterate_array(
    ii_json = li_json
    iv_path = '/' ).

  while li_iterator->has_next( ) = abap_true.
    li_item = li_iterator->next( ).
    append li_item->get( '/' ) to lt_values.
  endwhile.

  " lt_values contains 10, 20, 30 as strings
```

### Objects

Object members are visited in alphabetical order by name, rather than their order in the input JSON. The iterator returns each member's value as a slice; it does not return the member name.

```abap
  data li_json type ref to zif_ajson.
  data li_iterator type ref to zif_ajson_iterator.
  data li_item type ref to zif_ajson.
  data lt_values type string_table.

  li_json = zcl_ajson=>parse( '{"settings":{"b":2,"a":1}}' ).
  li_iterator = zcl_ajson_utilities=>iterate_object(
    ii_json = li_json
    iv_path = '/settings' ).

  while li_iterator->has_next( ) = abap_true.
    li_item = li_iterator->next( ).
    append li_item->get( '/' ) to lt_values.
  endwhile.

  " lt_values contains 1, 2 as strings (a before b)
```

If you also need the member names, use `members()` and read the corresponding paths on the original JSON instead.

### Empty containers and errors

- For an empty array or object, `has_next()` immediately returns `abap_false`.
- After the last item, `has_next()` returns `abap_false`; calling `next()` then returns an initial (unbound) reference.
- A missing path or the wrong container type raises `zcx_ajson_error` when creating the iterator. For example, `iterate_array()` rejects an object, and `iterate_object()` rejects an array. Handle this exception in the caller or declare it in the caller's `RAISING` clause, as with `parse()` in the examples above.
- An iterator moves forward only. Create a new iterator to visit the items again.

### Slices and changes to the source

Each returned item is a copy made by `slice()`, not a reference into the original JSON tree. Changes to the returned item do not update the source JSON.

The iterator itself traverses the source tree. Keep the source unchanged while iterating; if you need to modify it, collect the changes first and apply them after the loop. Iteration visits immediate children only: use another iterator on a returned slice to visit a nested array or object.
