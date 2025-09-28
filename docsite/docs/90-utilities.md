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

Sometimes you need to iterate through the object or array items. The "native" way to do it is to call `members()` on the node and then `loop` through the members concatenating original path and each memeber name (for array, the members would be array indexes - 1,2,3 ...). However, the utilities offer a more elegant approach.

```abap
  data li_iterator type ref to zif_ajson_iterator.
  li_iterator = zcl_ajson_utilities=>iterate_array(
    ii_json = li_json
    iv_path = '/path-to-array' ).
  while li_iterator->has_next( ) = abap_true.
    li_array_item = li_iterator->next( ).
    " returns slice of the original JSON
    " similar to li_json->slice( '/path-to-array/1' )

    " do what you need with the array item
  endwhile.
```

Similaryly, `iterate_object()` would iterate through the member nodes (in alphabetical order).
