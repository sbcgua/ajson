---
sidebar_position: 1
---

# Introduction

AJson is a json parser/serializer for ABAP designed with the convenience for developer in mind. It works with abap release 7.02 or higher.

## Features

- parse json content into a flexible form, not fixed to any predefined data structure, allowing to modify the parsed data, selectively access its parts and slice subsections of it
  - slicing can be particularly useful for REST header separation e.g. `{ "success": 1, "error": "", "payload": {...} }` where 1st level attrs are processed in one layer of your application and payload in another (and can differ from request to request)
- allows conversion to fixed abap structures/tables (`to_abap`)
- convenient interface to manipulate the data - `set( value )`, `set( structure )`, `set( table )`, `set( another_instance_of_ajson )`, also typed e.g. `set_date`
  - also `setx` for text-based value setting like `setx( '/a/b:123' )` (useful e.g. for constants in APIs or in unit-tests)
- seralization to string
- freezing (read only) instance content
- filtering - create a json skipping empty values, predefined paths, or your custom filter. *EXPERIMENTAL, interface may change*
- utility to calculate difference between 2 jsons

## Example of usage

```abap
data r type ref to zif_ajson.

r = zcl_ajson=>parse( '{"success": 1, "error": "", "payload": {"text": "hello"}}' ).

r->get( '/success' ).               " returns "1"
r->get_integer( '/success' ).       " returns 1 (number)
r->get_boolean( '/success' ).       " returns "X" (abap_true - because not empty)
r->get( '/payload/text' ).          " returns "hello"

r->members( '/' ).                  " returns table of "success", "error", "payload"

fragment = r->slice( '/payload' )
fragment->get( '/text' ).           " returns "hello" (the root has changed)

fragment->set(
  iv_path = '/text'
  iv_val  = 'new text' ).
fragment->stringify( ).             " {"text":"new text"}
```

See more examples and features in the further docs.
