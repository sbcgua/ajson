![abaplint](https://github.com/sbcgua/abap-string-map/workflows/abaplint/badge.svg)

# abap json (ajson)

Yet another json parser/serializer for ABAP. Should work on 7.31, maybe on 7.02 but with json sxml parser installed.

Features:
- parse into a flexible form, not fixed to any predefined data structure, allowing to change the parsed data and slice subsections of it
  - slicing can be particulary useful for REST header separation e.g. `{ success: 1, error: "", payload: {...} }` where `success` and Ko is processed in one layer of your application and payload in another (and can differ)
- allows conversion to fixed abap structures/tables (`to_abap`)
- convenient interface to manipulate the data (`set( value )`, `set( structure )`, `set( table )`, `set( another_instance_of_ajson )`)
- seralization to string (TODO)

Installed using [abapGit](https://github.com/larshp/abapGit)

## Examples and documentation

TODO

## References

- Forked from [here](https://github.com/abaplint/abaplint-abap-backend) originally
