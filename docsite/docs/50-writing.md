---
sidebar_position: 50
---

# Writing JSON values

The methods of interface allows setting attributes, objects, arrays.

## Individual value writing

```abap
data w type ref to zif_ajson.
w = zcl_ajson=>create_empty( ).

" Set value
" Results in { "a": { "b": { "num": 123, "str": "hello", "bool": true } } }
" The intermediary path is auto created, value type auto detected
w->set(
  iv_path = '/a/b/num'
  iv_val  = 123 ).
w->set(
  iv_path = '/a/b/str'
  iv_val  = 'hello' ).
w->set(
  iv_path = '/a/b/bool'
  iv_val  = abap_true ).
w->set(
  iv_path = '/a/b/str'
  iv_val  = 'escaping"\' ). " => "escaping\"\\", also with \n, \r, \t

" Ignoring empty values by default
w->set(
  iv_path = '/a'
  iv_val  = abap_false ). " => nothing added to json !!!
w->set(
  iv_ignore_empty = abap_false
  iv_path = '/a'
  iv_val  = abap_false ). " => "a": false
w->set(
  iv_path = '/a'
  iv_val  = 0 ). " => nothing added to json !!!
w->set(
  iv_ignore_empty = abap_false
  iv_path = '/a'
  iv_val  = 0 ). " => "a": 0

" With explicit type
w->set(
  iv_path      = '/a'
  iv_val       = '0'
  iv_node_type = 'num' ). " => "a": 0

```

## Individual TYPED values

```abap
" Set typed value
" IMPORTANTLY, empty values are always not ignored !
" Booleans -> converts not initial values to true
w->set_boolean(
  iv_path = '/a'
  iv_val  = 123 ). " => true
w->set_boolean( " empty value not ignored !
  iv_path = '/a'
  iv_val  = 0 ). " => false
w->set_boolean(
  iv_path = '/a'
  iv_val  = 'abc' ). " => true
w->set_boolean(
  iv_path = '/a'
  iv_val  = lt_non_empty_tab ). " => true

" Integer
w->set_integer( " this just forces conversion to int at param level
  iv_path = '/a'
  iv_val  = 123 ). " => 123
w->set_integer( " empty value not ignored !
  iv_path = '/a'
  iv_val  = 0 ). " => 0

" String (clike param)
w->set_string(
  iv_path = '/a'
  iv_val  = sy-datum ). " => e.g. 20200705
w->set_string( " empty value not ignored !
  iv_path = '/a'
  iv_val  = '' ). " => "a": ""

" Date - converts date param to json formatted date
w->set_date(
  iv_path = '/a'
  iv_val  = sy-datum ). " => e.g. "2020-07-05" (with dashes)

" Timestamp - converts timestamp param to json formatted data as ISO (<YYYY>-<MM>-<DD>T<HH>:<MM>:<SS>Z)
get time stamp field lv_timestamp.
w->set_timestamp(
  iv_path = '/a'
  iv_val  = lv_timestamp ). " => e.g. "2021-05-05T12-00-00Z" (with dashes)

" Null
" same effect is for initial data ref
w->set_null(
  iv_path = '/a' ). " => "a": null

```

## Text-based set

The method `setx` is a shortcut for full-scale `set`, it attempts to parse a string and detect both path and value from it. Although it is less performant (!) but it is more readable which can be beneficial for some cases where it is not critical e.g. setting constants in APIs or unit tests.  
**Format**: path and value are separated by `':'`, space around path and around value is trimmed.  

```abap
j->setx( '/a: 1' ).     " { "a": 1 }
j->setx( '/a: 1.123' ). " { "a": 1.123 }
j->setx( '/a: abc' ).   " { "a": "abc" }
j->setx( '/a: "abc"' ). " { "a": "abc" }
j->setx( '/a: "123"' ). " { "a": "123" } - force string
j->setx( '/a: null' ).  " { "a": null }
j->setx( '/a: true' ).  " { "a": true }
j->setx( '/a: false' ). " { "a": false }

" deep path are supported
j->setx( '/a/b/c: 1' ).

" and also arrays and objects
" Note, the object must be in complete json format, with "",
"   as such a value triggers full-scale parsing under the hood
j->setx( '/a: { "b": "abc" }' ). 
j->setx( '/a: [1,2,3]' ). 

" The method is chainable
j->setx( '/a: 1' )->setx( '/b: 2' ).
```

## Deletion and overwriting

```abap
" Importantly, values and whole branches are rewritten
" { "a": { "b": 0 } } - the old "b" completely deleted
w->set(
  iv_path = '/a/b'
  iv_val  = 0 ).

" Items can be deleted explicitly
w->delete( '/a/b' ). " => { "a": { } }

" Or completely cleared
w->clear( ).
```

## Settings objects

```abap
" Set object
" Results in { "a": { "b": { "payload": { "text": ..., "num": ... } } } }
data:
  begin of ls_payload,
    text type string,
    num type i,
  end of ls_payload.
w->set(
  iv_path = '/a/b/payload'
  iv_val  = ls_payload ).

" Set other object with ajson instance
w->set(
  iv_path = '/a/b/payload'
  iv_val  = lo_another_ajson ).
```

## Settings arrays/tables

```abap
" Set arrays
" Results in: { "array": [ "abc", "efg" ] }
" Tables of structures, of tables, and other deep objects are supported as well
data tab type string_table.
append 'abc' to tab.
append 'efg' to tab.
w->set(
  iv_path = '/array'
  iv_val  = tab ).

" Fill arrays item by item
" Different types ? no problem
w->push(
  iv_path = '/array'
  iv_val  = 1 ).
" => { "array": [ "abc", "efg", 1 ] }

w->push(
  iv_path = '/array'
  iv_val  = ls_payload ).
" => { "array": [ "abc", "efg", 1, { "text": ..., "num": ... } ] }

" Push verifies that the path item exists and is array
" it does NOT auto create path like "set"
" to explicitly create an empty array use "touch_array"
w->touch_array( '/array2' ).
" => { "array": ..., "array2": [] }
```

### Known issues

- removing an array item in the middle of array will not renumber the items

## Setting data refs

*TBD*

Currently not supported, but maybe in future. Except initial data ref which is equivalent to `set_null`.

- set null
- typed refs
- any data refs
- object refs

## Chaining

Set (and some other) methods also return `me` to support chaining: `li_json->set(...)->set(...)->touch_array(...)->push(...)`.

## Freezing JSON (read only)

It is possible to set an instance of ajson immutable (read only). It is done on object level with method `freeze` or at parse time with `iv_freeze = abap_true` param. This is one way only change. After this `set`, `delete`, `clear` and other modification methods will raise exceptions if used. Useful to freeze some kind of settings or service responses.

## Rendering to JSON string

`zcl_ajson` instance content can be rendered to JSON string using `zif_ajson~stringify` method (also has alias at class level). It also supports optional indentation.

```abap

    data lo_json type ref to zcl_ajson.
    data li_json type ref to zif_ajson.

    lo_json   = zcl_ajson=>create_empty( ).
    li_json = lo_json.

    li_json->set(
      iv_path = '/a'
      iv_val  = 1 ).
    li_json->set(
      iv_path = '/b'
      iv_val  = 'B' ).
    li_json->touch_array(
      iv_path = '/e' ).
    li_json->touch_array(
      iv_path = '/f' ).
    li_json->push(
      iv_path = '/f'
      iv_val  = 5 ).

    data lv type string.
    lv = lo_json->stringify( ). " or li_json->stringify( ).
    " {"a":1,"b":"B","e":[],"f":[5]}

    lv = lo_json->stringify( iv_indent = 2 ). " indent with 2 spaces
    " {
    "   "a": 1,
    "   "b": "B",
    "   "e": [],
    "   "f": [
    "     5
    "   ]
    " }
```

## Keep item order

Sometimes you may want to keep order of JSON items in the same order as it was in ABAP structure (assuming you `set` structures or table of structures). To do this: set `iv_keep_item_order` flag when creating an instance or call `keep_item_order` after creation of instance, before any `set`.

```abap
  data:
    begin of ls_dummy,
      zulu type string,
      alpha type string,
      beta type string,
    end of ls_dummy.

  li_json->keep_item_order( ).
  li_json->set(
    iv_path = '/'
    iv_val  = ls_dummy ).
  li_json->stringify( ). " '{"zulu":"z","alpha":"a","beta":"b"}'
  " otherwise - '{"alpha":"a","beta":"b","zulu":"z"}'

  " OR
  li_json = zcl_ajson=>new( iv_keep_item_order = abap_true ).
  ...
```

The same parameter exists for parsing

```abap
  li_json = zcl_ajson=>parse( 
    iv_json            = '{"b":1,"a":2}'
    iv_keep_item_order = abap_true ).
  li_json->stringify( ). " '{"b":1,"a":2}'
```

## Other

### Checking current instance behavior options

Behavior options like `read_only` or `keep_item_order` are accessible via `opts()` method (returns `zif_ajson=>ty_opts`).
