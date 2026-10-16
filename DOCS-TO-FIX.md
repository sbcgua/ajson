**High Impact**

- Date/time docs say formatting is off by default, but code defaults it on.
  [60-date-time.md](C:/Users/at/Documents/devs/abap/ajson/docsite/docs/60-date-time.md:9) says ABAP date/time formats are default until `format_datetime`; [zcl_ajson.clas.abap](C:/Users/at/Documents/devs/abap/ajson/src/core/zcl_ajson.clas.abap:22) and line 28 default `iv_format_datetime = abap_true`.

- Docs claim timestamp timezone offsets support `-hh:mm`, but code regex only accepts `+hh:mm`.
  [60-date-time.md](C:/Users/at/Documents/devs/abap/ajson/docsite/docs/60-date-time.md:37) documents `Z or +hh:mm or -hh:mm`; [zcl_ajson.clas.locals_imp.abap](C:/Users/at/Documents/devs/abap/ajson/src/core/zcl_ajson.clas.locals_imp.abap:1230) uses a plus-only offset regex, despite later having a `when '-'` branch.

- Data refs writing docs are stale.
  [50-writing.md](C:/Users/at/Documents/devs/abap/ajson/docsite/docs/50-writing.md:212) says data refs are not supported except initial refs, but code and tests support non-initial refs, including refs inside structures: [zcl_ajson.clas.locals_imp.abap](C:/Users/at/Documents/devs/abap/ajson/src/core/zcl_ajson.clas.locals_imp.abap:1744), [testclasses](C:/Users/at/Documents/devs/abap/ajson/src/core/zcl_ajson.clas.testclasses.abap:3128).

- Data-ref reading example appears invalid.
  [40-reading.md](C:/Users/at/Documents/devs/abap/ajson/docsite/docs/40-reading.md:168) uses `new zcl_ajson=>new( ii_refs_initiator = li_refs )`, but `zcl_ajson=>new` has no `ii_refs_initiator`; the parameter belongs to `to_abap` in [zif_ajson.intf.abap](C:/Users/at/Documents/devs/abap/ajson/src/core/zif_ajson.intf.abap:145).

**Doc/API Mismatches**

- Mapping custom implementation example uses the old method name `rename_field`; the interface method is `rename_node`.
  Docs: [70-mapping.md](C:/Users/at/Documents/devs/abap/ajson/docsite/docs/70-mapping.md:27). Code: [zif_ajson_mapper.intf.abap](C:/Users/at/Documents/devs/abap/ajson/src/core/zif_ajson_mapper.intf.abap:16).

- README has stale breaking-change placeholders and removed/absent API names.
  [README.md](C:/Users/at/Documents/devs/abap/ajson/README.md:17) still has `TODO CHECK changelog`; lines 21-23 mention deprecated/old names like `create_empty`, `create_from`, `create_field_mapping`, `create_camel_case`.

- `set_timestamp` doc example uses invalid ISO time separators.
  [50-writing.md](C:/Users/at/Documents/devs/abap/ajson/docsite/docs/50-writing.md:99) shows `2021-05-05T12-00-00Z`; code formats as `HH:MM:SSZ`.

- Filtering docs reference `zif_ajson=>ty_node`, but the type lives in `zif_ajson_types=>ty_node`.
  Docs: [80-filtering.md](C:/Users/at/Documents/devs/abap/ajson/docsite/docs/80-filtering.md:56). Code: [zif_ajson_filter.intf.abap](C:/Users/at/Documents/devs/abap/ajson/src/core/zif_ajson_filter.intf.abap:16).

**Underdocumented Areas**

- `parse` accepts more than a JSON string: `char`, `string`, `string_table`, and `xstring`; tests also cover bare scalar JSON values. Docs mostly say `lv_json_string`. See [locals_imp](C:/Users/at/Documents/devs/abap/ajson/src/core/zcl_ajson.clas.locals_imp.abap:234) and tests around [parse_bare_values](C:/Users/at/Documents/devs/abap/ajson/src/core/zcl_ajson.clas.testclasses.abap:110).

- Public methods missing or barely covered: `get_number`, `get_timestamp`, `get_timestampl`, `array_to_string_table`, and static `normalize_path`. These are in [zif_ajson.intf.abap](C:/Users/at/Documents/devs/abap/ajson/src/core/zif_ajson.intf.abap:106).

- Utility docs list `merge` but do not explain semantics. Tests show it does not overwrite existing values and appends new array values: [zcl_ajson_utilities.clas.testclasses.abap](C:/Users/at/Documents/devs/abap/ajson/src/libs/zcl_ajson_utilities.clas.testclasses.abap:418).

- Utility methods can accept existing `zif_ajson` instances, not only JSON strings, and `diff`/`merge` have `iv_keep_empty_arrays`. Docs do not really expose this surface: [zcl_ajson_utilities.clas.abap](C:/Users/at/Documents/devs/abap/ajson/src/libs/zcl_ajson_utilities.clas.abap:12).

**Potential Improvements**

- Consider preserving options in `slice`. `clone`, `filter`, and `map` copy options, but `slice` creates a fresh `zcl_ajson` with defaults: [zcl_ajson.clas.abap](C:/Users/at/Documents/devs/abap/ajson/src/core/zcl_ajson.clas.abap:845).

- Clarify freeze behavior for derived copies. `clone` copies several options but not `read_only`; docs say freeze is one-way “on object level”, but users may expect cloned/filtered/mapped results to remain immutable.

- Clean doc typos while touching docs: “seralization”, “comparation”, “consumtion”, “memeber”, “Similaryly”.