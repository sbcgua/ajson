---
sidebar_position: 40
---

# Reading JSON values

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

Performance tip: note, that slice creates a copy of the json tree, and so increases memory consumtion which may be an issue in some cases.

## Getting node type

In some case you might want to know node type prior to accessing it first. Type can be 'str', 'num', 'null', 'bool', 'object', 'array'.

```abap
r->get_node_type( '/payload/false' ).         " returns "bool"
r->get_node_type( '/payload/text' ).          " returns "str"
```

## Converting to abap structure

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

`to_abap` supports transferring "corresponding only" fields.

```abap
payload->to_abap( 
  exporting iv_corresponding = abap_true
  importing ev_container     = ls_payload ).

" Or via an instance flag (persists after setting!)
payload->to_abap_corresponding_only( )->to_abap( importing ev_container = ls_payload ).
```

`to_abap` supports creating data references. **TBD**
