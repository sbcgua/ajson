---
sidebar_position: 60
---

# Formatting date/time values

## Auto format date/time

By default date, time and timestamp dates are not formatted and are written in ABAP format as 'YYYYMMDD', 'HHMMSS'. This can be changed by calling `format_datetime` method after creation. After that the date/time will be auto-formatted as 'YYYY-MM-DD' and 'HH:MM:SS' respectively. **Important: this may become the default behavior in future version**

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
- ABAP base type of field is P (Packed)
- if the ABAP field has data element `timestampl`, then fractions of seconds are supported

### Examples

Using a JSON with possible formats:

```json
{
  "date":"2020-07-28",
  "datetime":"2020-07-28T00:00:00",
  "datetime_utc":"2020-07-28T00:00:00Z",
  "datetime_plus1":"2020-07-28T01:00:00+01:00",
  "datetime_fract":"2020-07-28T00:00:00.123456Z",
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
      datetime_fract TYPE timestampl, " long timestamp
    END OF json_timestamp.

  DATA(lo_ajson) = zcl_ajson=>parse( json_content ).

  lo_ajson->to_abap( IMPORTING ev_container = json_timestamp ).
```
