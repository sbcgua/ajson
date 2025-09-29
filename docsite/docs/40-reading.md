---
sidebar_position: 40
---

# Reading JSON values

The methods of interface allows accessing attributes and converting to ABAP structure.

Examples below assume original JSON
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

## Individual value reading

```abap
data r type ref to zif_ajson.
r = zcl_ajson=>parse( lv_json_string_from_above ).

r->is_empty( ).                     " returns abap_false
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

r->members( '/' ).                  " returns table of "success", "error", "payload"
```

## Segment slicing

```abap
" Slice returns zif_ajson instance but "payload" becomes root
" Useful to process API responses with unified wrappers
data payload type ref to zif_ajson.
payload = r->slice( '/payload' ). 
```

Performance tip: note, that slice creates a copy of the JSON tree, and so increases memory consumtion which may be an issue in some cases.

## Getting node type

In some case you might want to know node type prior to accessing it first. Type can be 'str', 'num', 'null', 'bool', 'object', 'array'.

```abap
r->get_node_type( '/payload/false' ).         " returns "bool"
r->get_node_type( '/payload/text' ).          " returns "str"
```

## Converting to ABAP structure

Instead of reading individual values, you can use `to_abap` for converting the JSON data into a corresponding ABAP structure. The types are mapped as follows:

| JSON     | ABAP                | Expected JSON format |
|----------|---------------------|----------------------|
| str      | date (d)            | "YYYY-MM-DD" or "YYYY-MM-DDT" (year, month, day, optional fixed "T")  |
| str      | time (t)            | "HH:MM:SS" or "HH:MM:SST" (hour, minute, seconds, optional "T")       |
| str      | timestamp (p 15,0)  | "YYYY-MM-DDTHH:MM:SSZ" or "YYYY-MM-DDTHH:MM:SS+XX:YY" (with timezone) |
| str      | timestampl (p 21,7) | "YYYY-MM-DDTHH:MM:SS.FFFFFFFZ" or "YYYY-MM-DDTHH:MM:SS.FFFFFFF+XX:YY" (with timezone) |
| str      | utclong             | "YYYY-MM-DDTHH:MM:SS.FFFFFFFZ" or "YYYY-MM-DDTHH:MM:SS.FFFFFFF+XX:YY" (with timezone) |
| str      | other               | Standard ABAP value mapping | 
| num      | any                 | Standard ABAP value mapping |
| bool     | abap_bool (c 1)     | true, false                 |
| null     | -                   | null (ignored)              |
| object   | structure           |                             |
| array    | table               |                             |

Except for the special date/time mappings, JSON data will be assigned to the corresponding ABAP fields using standard ABAP [data conversion rules](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenconversion_elementary.htm). Look out for conversion errors which throw `CX_SY_CONVERSION_ERROR` or unintentionally truncating values if a target field is shorter than the value.


```abap
data:
  begin of ls_payload,
    text  type string,
    num   type i,
    bool  type abap_bool,
    false type abap_bool,
    null  type string,
    table type string_table, " Array !
  end of ls_payload.

payload->to_abap( importing ev_container = ls_payload ).
```

`to_abap` supports transferring "corresponding only" fields. This will ignore JSON attributes that do not have a matching ABAP field name (see [Mapping](70-mapping.md)).

```abap
payload->to_abap( 
  exporting iv_corresponding = abap_true
  importing ev_container     = ls_payload ).

" Or via an instance flag (persists after setting!)
payload->to_abap_corresponding_only( )->to_abap( importing ev_container = ls_payload ).
```

`to_abap` supports creating data references. You can determine the type of a field in the ABAP structure at runtime. For this to work, define the field as `type ref to data` and initialize the reference using `zcl_ajson_ref_initializer_lib=>create_path_refs_init` for each path to a reference field. 

Example:

```json
// 1st JSON
{ "tableName": "T002", "tableContent": { ...structure of table T002... } }
// 2nd JSON
{ "tableName": "T100", "tableContent": { ...structure of table T100... } }
```

```abap
data:
  begin of ls_data,
    table_name    type string,
    table_content type ref to data,
  end of ls_data.

data:
  ls_t002   type t002,
  ls_t100   type t100,
  li_refs   type ref to zif_ajson_ref_initializer,
  lt_refs   type zif_ajson_ref_initializer=>tty_data_refs,
  ls_refs   like line of lt_refs.

do 2 times.
  " prepare mapping of paths to data references
  clear: ls_refs, lt_refs.

  case sy-index.
    when 1.
      " 1st JSON: reference T002 structure
      get reference of ls_t002 into ls_refs-dref.
    when 2.
      " 2nd JSON: reference T100 structure
      get reference of ls_t100 into ls_refs-dref.
  endcase.

  ls_refs-path = '/'.
  ls_refs-name = 'table_content'.
  insert ls_refs into table lt_refs.

  " create json instance with ref initializer
  li_refs = zcl_ajson_ref_initializer_lib=>create_path_refs_init( lt_refs ).

  lo_json = new zcl_ajson=>new( ii_refs_initiator = li_refs ).
  lo_json->to_abap( importing ev_container = ls_data ).

  " ...

enddo.
```
