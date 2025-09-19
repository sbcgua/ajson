<!-- markdownlint-disable first-line-heading -->
![abaplint](https://github.com/sbcgua/ajson/workflows/abaplint/badge.svg)
![abap package version](https://img.shields.io/endpoint?url=https://abap-version-shield.sbcg.com.ua/version-shield-json/github/sbcgua/ajson/src/core/zif_ajson.intf.abap)

# abap json (ajson)

Yet another json parser/serializer for ABAP. It works with release 7.02 or higher.

- [Documentation](https://sbcgua.github.io/ajson) (or also [here](./docsite/docs))
- [Blog articles](https://sbcgua.github.io/ajson/blog)
- [Change log](./changelog.txt)

## Recent breaking changes

Since v1.2.0: **TODO**

- there are changes in mapper interface, see [Mapping (field renaming)](#mapping-field-renaming) section below. In essence, implement `rename_node` method if needed, `to_json` and `to_abap` will be deprecated. As well as `create_field_mapping` and `create_camel_case` mappers
- potentially `create_empty` static method may be deprecated. It is considered to use `new` instead (and/or direct creation `create object`). Under consideration, post an issue if you have an opinion on this subject.
- also `create_from` is potentially suboptimal, so prefer `clone`, `filter` and `map` instead.

## Features

- parse into a flexible form, not fixed to any predefined data structure, allowing to modify the parsed data, selectively access its parts and slice subsections of it
  - slicing can be particularly useful for REST header separation e.g. `{ "success": 1, "error": "", "payload": {...} }` where 1st level attrs are processed in one layer of your application and payload in another (and can differ from request to request)
- allows conversion to fixed abap structures/tables (`to_abap`)
- convenient interface to manipulate the data - `set( value )`, `set( structure )`, `set( table )`, `set( another_instance_of_ajson )`, also typed e.g. `set_date`
  - also `setx` for text-based value setting like `setx( '/a/b:123' )` (useful e.g. for constants in APIs or in unit-tests)
- seralization to string
- freezing (read only) instance content
- filtering. Create a json skipping empty values, predefined paths, or your custom filter.
- mapping - rule-based changing node names (e.g. snake case to camel case, upper/lower case)
- utility to calculate difference between 2 jsons

Installed using [abapGit](https://github.com/abapGit/abapGit), see also [Installation](https://sbcgua.github.io/ajson/docs/installation) section of the documentation.

## Example of usage

```abap
data r type ref to zif_ajson.
data fragment type ref to zif_ajson.

r = zcl_ajson=>parse( '{"success": 1, "error": "", "payload": {"text": "hello"}}' ).

r->get( '/success' ).              " returns "1"
r->get_integer( '/success' ).      " returns 1 (number)
r->get_boolean( '/success' ).      " returns "X" (abap_true - because not empty)
r->get( '/payload/text' ).         " returns "hello"

r->members( '/' ).                 " returns table of "success", "error", "payload"

fragment = r->slice( '/payload' ).
fragment->get( '/text' ).          " returns "hello" (the root has changed)

fragment->set(
  iv_path = '/text'
  iv_val  = 'new text' ).
fragment->stringify( ).            " {"text":"new text"}
```

See a lot more examples and usages in the [Documentation](https://sbcgua.github.io/ajson).

## References

- Forked from [abaplint-abap-backend](https://github.com/abaplint/abaplint-abap-backend) originally, at early stages
- The package is unit tested dynamically in Github actions thanks to the [abap transpiler](https://github.com/abaplint/transpiler) by @larshp
- Publication at SCN:
  - [Bicycles. #2 – AJSON – yet another abap json parser and serializer](https://blogs.sap.com/2020/08/14/bicycles.-2-ajson-yet-another-abap-json-parser-and-serializer)
  - [JSON alternative to maintenance views in ABAP](https://blogs.sap.com/2022/07/23/json-alternative-to-maintenance-views-in-abap/)
  - Both above and future articles are in the [Blog articles](https://sbcgua.github.io/ajson/blog)
