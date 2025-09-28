---
slug: ajson-yet-another-abap-json-parser-and-serializer
title: Ajson - yet another abap json parser and serializer
authors: sbcgua
tags: [old]
---

Once I read a comment by Paul Hardy (if I remember well) to a blog post that "Every week someone publishes a post about own excel or json parser". I don't remember the exact wording, but think the essence is clear. So I proudly present one more proof to that statement 🙂

Why re-invent an abap json parser? There are plenty of other parsers already, but I didn't find exactly what I wanted. Let's list some of widely known approaches.

<!-- truncate -->

- default transformation 7.4 (_standard, but far from convenience_)
- /ui2/cl_json
- /ui5/cl_json_parser
- [https://github.com/se38/zJSON](https://github.com/se38/zJSON) (_classics! :_)
- [https://github.com/koemaeda/abap-json](https://github.com/koemaeda/abap-json) (_this seems to be unfairly missing in searches and references, I personally found this one very good and compatible. also it is bi-directional_)

The above libraries are cool. Nothing wrong with them. But they share one approach - take json string, convert it into an abap structure/table and vice versa (except UI5 one). While working with web APIs I faced a couple of specifics which are not easily addressed with this approach.

- separating API response parts - properly designed REST APIs often wrap the payload data into a typical API header e.g. `{ success: 1, error: "xyz", payload: ... }`. I wanted to separate processing of API response layer from the payload processing. So to "extract" payload as a root node and clean it from header stuff for the further processing
- flexibity - APIs may change, especially if the other side is an evolving custom service. Changing the abap structures to match is a pain
- partial data extraction - what if I don't need the whole json data structure, but only "one array there", why should I reflect the whole API structure in abap?

So I wanted a json parser that would be:

- 2-directional
- easy to install (no OSS notes)
- compatible with 702/731
- convenient to use in terms of code (oh, that's important!)
- able to convert to/from abap
- able to access parts of json specifically without complete structure conversion

After trying some alternatives I created my own implementation to match my need and coding convenience I wanted. Let alone the fact that it was a fun (which is quite important!). **The class can** read json, access it's parts flexibly, slice (extract) parts, make the json immutable (if required), set json attributes/arrays, convert the data to and from abap structures and serialize all that back to json with or without indentation. If I'm not lazy (or if there will be a practical need) I might also implement json schema validation.

Bellow are some examples which should be self explainable.

The code is open-sourced, heavily covered with unit tests and published under MIT license here: [https://github.com/sbcgua/ajson](https://github.com/sbcgua/ajson). Can be installed using [abapGit](https://github.com/abapGit/abapGit).

## Functionality and Examples

### Basics

- To parse existing json data - call `zcl_ajson=>parse( lv_json_string )`
- To create a new empty json instance (to set values and serialize) - call `zcl_ajson=>create_empty( )`
- Json attributes are addressed by path in form `/obj1/obj2/value` of e.g. `/a/b/c` addresses `{ a: { b: { c: "this value !" } } }`
- Array items addressed with index starting from 1: `/tab/2/val` -> `{ tab: \[ {...}, { val: "this value !" } \] }`

### JSON reader

The reading is done via `zif_ajson_reader` interface, or aliases of the main class. The methods of interface allows accessing attributes and converting to abap structure. (_**Update**: `zif_ajson_reader` and `zif_ajson_writer` were unified into one interface `zif_ajson`_)

Examples below assume original json was:

```json
{
  "success": 1,
  "error": "",
  "payload": {
    "text": "hello",
    "num": 123,
    "bool": true,
    "false": false,
    "null": null,
    "date": "2020-07-28",
    "table": [
      "abc",
      "def"
    ]
  }
}
```

#### Individual values

```abap
data r type ref to zif_ajson_reader.

r = zcl_ajson=>parse( lv_json_string_from_above ).
r->exists( '/success' ).            " returns abap_true
r->get( '/success' ).               " returns "1"
r->get_integer( '/success' ).       " returns 1 (number)
r->get_boolean( '/success' ).       " returns "X" (abap_true - because not empty)
r->get( '/payload/bool' ).          " returns "true"
r->get_boolean( '/payload/bool' ).  " returns "X" (abap_true)
r->get( '/payload/false' ).         " returns "false"
r->get_boolean( '/payload/false' ). " returns "" (abap_false)
r->get( '/payload/null' ).          " returns "null"
r->get_string( '/payload/null' ).   " returns "" (empty string)
r->get( '/payload/date' ).          " returns "2020-07-28"
r->get_date( '/payload/date' ).     " returns "20200728" (type d)
r->members( '/' ).                  " returns string table of "success", "error", "payload"
```

#### Segment slicing

```abap
" Slice returns zif_ajson_reader instance but "payload" becomes root
" Useful to process API responses with unified wrappers

data payload type ref to zif_ajson_reader.
payload = r->slice( '/payload' ).
payload->get( '/text' ). " Now the root has changed
```

#### Converting to abap structure

```abap
data:
  begin of ls_payload,
    text type string,
    num type i,
    bool type abap_bool,
    false type abap_bool,
    null type string,
    table type string_table, " Array !
  end of ls_payload.

payload->to_abap( importing ev_container = ls_payload ).
```

### JSON writer

Modification of JSON is accessible via `zif_ajson_writer` interface of directly via aliases in the main class. The methods of interface allows setting attributes, objects, arrays. (_**Update**: `zif_ajson_reader` and `zif_ajson_writer` were unified into one interface `zif_ajson`_)

#### Individual nodes

```abap
data w type ref to zif_ajson_writer.

w = zcl_ajson=>create_empty( ).

" Set value
" Results in { a: { b: { num: 123, str: "hello", bool: true } } }
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
```

#### Individual TYPED values

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

w->set_string(
  iv_path = '/a'
  iv_val  = sy-datum ). " => e.g. "2020-07-05" (with dashes)

" Null
" same effect is for initial data ref

w->set_null(
  iv_path = '/a' ). " => "a": null
```

#### Deletion and rewriting

```abap
" Importantly, values and whole branches are rewritten
" { a: { b: 0 } } - the old "b" completely deleted

w->set(
  iv_path = '/a/b'
  iv_val  = 0 ).

" Items can be deleted explicitly

w->delete( '/a/b' ). " => { a: { } }

" Or completely cleared

w->clear( ).
```

#### Settings objects

`set` method is smart - it detects the type of input value automatically.

```abap
" Set object
" Results in { a: { b: { payload: { text: ..., num: ... } } } }

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

#### Settings arrays/tables

```abap
" Set arrays
" Results in: { array: [ "abc", "efg" ] }
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

" => { array: [ "abc", "efg", 1 ] }

w->push(
  iv_path = '/array'
  iv_val  = ls_payload ).

" => { array: [ "abc", "efg", 1, { text: ..., num: ... } ] }

" Push verifies that the path item exists and is array
" it does NOT auto create path like "set"
" to explicitly create an empty array use "touch_array"

w->touch_array( '/array2' ).
" => { array: ..., array2: [] }
```

#### Freezing json (read only)

It is possible to set an instance of ajson immutable (read only). It is done on object level with method `freeze` or at parse time with `iv_freeze = abap_true` param. This is one way only change. After this `set`, `delete`, `clear` and other modification methods will raise exceptions if used. Useful to freeze some kind of settings or service responses to avoid subtle bugs.

### Rendering to a JSON string

`zcl_ajson` instance content can be rendered to JSON string using `stringify` method. It also supports optional indentation.

```abap
data lo_json type ref to zcl_ajson.
data li_writer type ref to zif_ajson_writer.

lo_json   = zcl_ajson=>create_empty( ).
li_writer = lo_json.

li_writer->set(
  iv_path = '/a'
  iv_val  = 1 ).
li_writer->set(
  iv_path = '/b'
  iv_val  = 'B' ).
li_writer->touch_array(
  iv_path = '/e' ).
li_writer->touch_array(
  iv_path = '/f' ).
li_writer->push(
  iv_path = '/f'
  iv_val  = 5 ).

data lv type string.

lv = lo_json->stringify( ).
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

## P.S. ...one more use case

One more use case I'd like to share. My team and I are developing custom SAP related products and at the beginning of development it is not visible what kind of customizing will be required and the shape of it. And if you need to change the shape of settings after the code is written (due to a request from a customer or due to mere evolution of the product) you have to change tables, maintenance views, and all this stuff which is a headache. Recently, we've been experimenting with approach used by abapGit - a table with a string field with freely definable XML (JSON in our case). This enable flexibility which is quite handy for developing of quickly evolving code. The solution maybe arguable, but it looks very convenient so far. I might write a separate blog post on the subject and maybe even publish some code, if there is interest.

I hope you find this useful or at least interesting. 🙂

P.S. _Originally posted at [SAP Community platform](https://community.sap.com/t5/application-development-and-automation-blog-posts/bicycles-2-ajson-yet-another-abap-json-parser-and-serializer/ba-p/13464029) on 2020-Aug-14._
