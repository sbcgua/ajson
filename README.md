![abaplint](https://github.com/sbcgua/ajson/workflows/abaplint/badge.svg)
![abap package version](https://img.shields.io/endpoint?url=https://shield.abap.space/version-shield-json/github/sbcgua/ajson/src/zif_ajson.intf.abap)

# abap json (ajson)

Yet another json parser/serializer for ABAP. It works with release 7.02 or higher.

**BREAKING CHANGES in v1.1**
- `zif_ajson_reader` and `zif_ajson_writer` interface removed. Use `zif_ajson`. The last version with those interfaces is *v1.0.4*.

Features:
- parse into a flexible form, not fixed to any predefined data structure, allowing to modify the parsed data, selectively access its parts and slice subsections of it
  - slicing can be particularly useful for REST header separation e.g. `{ "success": 1, "error": "", "payload": {...} }` where 1st level attrs are processed in one layer of your application and payload in another (and can differ from request to request)
- allows conversion to fixed abap structures/tables (`to_abap`)
- convenient interface to manipulate the data - `set( value )`, `set( structure )`, `set( table )`, `set( another_instance_of_ajson )`, also typed e.g. `set_date`
- seralization to string
- freezing (read only) instance content
- filtering. Create a json skipping empty values, predefined paths, or your custom filter. *EXPERIMENTAL, interface may change*
- utility to calculate difference between 2 jsons

Installed using [abapGit](https://github.com/larshp/abapGit)

## Examples and documentation

### Instantiating and basics

- To parse existing json data - call `zcl_ajson=>parse( lv_json_string )`
- To create a new empty json instance (to set values and serialize) - call `zcl_ajson=>create_empty( )`
- All functional methods and types are defined via `zif_ajson` interface. Methods have alias in the `zcl_ajson` class, however please restrain from using them directly as they may be *deprecated* in future.
- Json attributes are addressed by path in form `/obj1/obj2/value` of e.g. `/a/b/c` addresses `{ "a": { "b": { "c": "this value !" } } }`
- Array items addressed with index starting from 1: `/tab/2/val` -> `{ "tab": [ {...}, { "val": "this value !" } ] }`
- Mapping and formatting options are available with interface `zif_ajson_mapping`. Predefined types for field mapping (ABAP <=> JSON), Camel Case, UPPER/lower case from class `zcl_ajson_mapping`

### JSON reading

The methods of interface allows accessing attributes and converting to abap structure.

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
data r type ref to zif_ajson.
r = zcl_ajson=>parse( lv_json_string_from_above ).

r->exists( '/success' ).              " returns abap_true

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

r->members( '/' ).                    " returns table of "success", "error", "payload"
```

#### Segment slicing

```abap
" Slice returns zif_ajson instance but "payload" becomes root
" Useful to process API responses with unified wrappers
data payload type ref to zif_ajson.
payload = r->slice( '/payload' ). 
```

#### Getting node type

In some case you might want to know node type prior to accessing it first. Type can be 'str', 'num', 'null', 'bool', 'object', 'array'.

```abap
r->get_node_type( '/payload/false' ).         " returns "bool"
r->get_node_type( '/payload/text' ).          " returns "str"
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

### JSON writing

The methods of interface allows setting attributes, objects, arrays.

#### Individual values

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

#### Deletion and rewriting

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

#### Settings objects

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

#### Settings arrays/tables

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

#### Setting data refs

Currently not supported, but maybe in future. Except initial data ref which is equivalent to `set_null`.

#### Chaining

Set (and some other) methods also return `me` to support chaining: `li_json->set(...)->set(...)->touch_array(...)->push(...)`.

#### Freezing JSON (read only)

It is possible to set an instance of ajson immutable (read only). It is done on object level with method `freeze` or at parse time with `iv_freeze = abap_true` param. This is one way only change. After this `set`, `delete`, `clear` and other modification methods will raise exceptions if used. Useful to freeze some kind of settings or service responses.

### Rendering to JSON string

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

#### Keep item order

Sometimes you may want to keep order of json items in the same order as it was in abap structure (assuming you `set` structures or table of structures). To do this call `keep_item_order` after creation of instance, before any `set`.

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
```

#### Auto format date/time

By default date, time and timestamp dates are not formatted and are written in abap format as 'YYYYMMDD', 'HHMMSS'. This can be changed by calling `format_datetime` method after creation. After that the date/time will be auto-formatted as 'YYYY-MM-DD' and 'HH:MM:SS' respectively. **Important: this may become the default behavior in future version**

```abap
  data:
    begin of ls_dummy,
      date type d value '20220412',
    end of ls_dummy.

  li_json->format_datetime( ).
  li_json->set(
    iv_path = '/'
    iv_val  = ls_dummy ).
  li_json->stringify( ). " {"date":"2022-04-12"}'
  " otherwise - {"date":"20220412"}

```

## Timestamps

Conversion from JSON to ABAP can determine automatically if the value is a timestamp if:
- value has timestamp format YYYY-MM-DDThh:mm:ssTZD, where
  - YYYY = four-digit year
  - MM = two-digit month (01=January, etc.)
  - DD = two-digit day of month (01 through 31)
  - hh = two digits of hour (00 through 23) (am/pm NOT allowed)
  - mm = two digits of minute (00 through 59)
  - ss = two digits of second (00 through 59)
  - TZD = time zone designator (Z or +hh:mm or -hh:mm)
- abap base type of field is P (Packed)

### Examples

Using a json with possible formats:
```json
{
  "date":"2020-07-28",
  "datetime":"2020-07-28T00:00:00",
  "datetime_utc":"2020-07-28T00:00:00Z",
  "datetime_plus1":"2020-07-28T01:00:00+01:00"
}
```

Can be mapped to following structure:
```abap
  DATA:
    BEGIN OF json_timestamp,
      date           TYPE d,
      datetime       TYPE timestamp,
      datetime_utc   TYPE timestamp,
      datetime_plus1 TYPE timestamp,
    END OF json_timestamp.

  DATA(lo_ajson) = zcl_ajson=>parse( json_content ).

  lo_ajson->to_abap( IMPORTING ev_container = json_timestamp ).
```

## Mapping / Formatting JSON

The interface `zif_ajson_mapping` allows to create custom mapping for ABAP and JSON fields.

Some mappings are provided by default:
- ABAP <=> JSON mapping fields
- JSON formatting to Camel Case
- JSON formatting to UPPER/lower case

### Example: JSON => ABAP mapping fields

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

### Example: ABAP => JSON mapping fields

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

### Example: Camel Case - To JSON (first letter lower case)

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

### Example: Camel Case - To JSON (first letter upper case)

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

### Example: Camel Case - To ABAP

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

### Example: Lower Case - To JSON

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

### Example: Upper Case - To JSON

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

## Filtering

*This is an experimental feature, the interface may change*

This feature allows creating a json from existing one skipping some nodes. E.g. empty values, predefined paths or using your custom filter.

### Predefined filters

Remove empty values
```abap
  " li_json_source: { "a":1, "b":0, "c":{ "d":"" } }
  li_json_filtered = zcl_ajson=>create_from(
    ii_source_json = li_json_source
    ii_filter = zcl_ajson_filter_lib=>create_empty_filter( ) ).
  " li_json_filtered: { "a":1 }
```

Remove predefined paths
```abap
  " li_json_source: { "a":1, "b":0, "c":{ "d":"" } }
  li_json_filtered = zcl_ajson=>create_from(
    ii_source_json = li_json_source
    ii_filter = zcl_ajson_filter_lib=>create_path_filter( 
      it_skip_paths = value #( ( '/b' ) ( '/c' ) ) ) ).
  " li_json_filtered: { "a":1 }
  
  " OR
  ...
  zcl_ajson_filter_lib=>create_path_filter( iv_skip_paths = '/b,/c' ).
  ...
```

"AND" filter
```abap
  ...
  zcl_ajson_filter_lib=>create_and_filter( value #(
    ( zcl_ajson_filter_lib=>create_empty_filter( ) )
    ( zcl_ajson_filter_lib=>create_path_filter( iv_skip_paths = '/xyz' ) )
  ) ).
  ...
```

### Custom filters

In order to apply a custom filter you have to implement a class with `zif_ajson_filter` interface. The interface has one method `keep_node` which receives `is_node` - json tree node of `zif_ajson=>ty_node` type and also the `iv_visit` param. `iv_visit` will be `zif_ajson_filter=>visit_type-value` for all final leafs (str,num,bool,null) and will get `visit_type-open` or `visit_type-close` values for objects and arrays. So the objects and arrays will be called twice - before and after filtering - this allows examining their children number before and after the current filtering. For example of implementation see local implementations of `zcl_ajson_filter_lib` class.

## Utilities

Class `zcl_ajson_utilities` provides the following methods:
- `diff` - returns all inserts, deletions, and changes between two JSON objects
- `sort` - returns JSON string with nodes sorted alphabetically

### Difference between JSON

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

### Sorting of JSON object or string

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

## Known issues

- removing an array item in the middle of array will not renumber the items

## References

- Forked from [here](https://github.com/abaplint/abaplint-abap-backend) originally, at early stages
- Publication at SCN - https://blogs.sap.com/2020/08/14/bicycles.-2-ajson-yet-another-abap-json-parser-and-serializer
