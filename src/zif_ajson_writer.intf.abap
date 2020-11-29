interface zif_ajson_writer
  public .

  methods clear
    raising
      zcx_ajson_error.

  methods set
    importing
      iv_path type string
      iv_val type any
      iv_ignore_empty type abap_bool default abap_true
    raising
      zcx_ajson_error.

  methods set_boolean
    importing
      iv_path type string
      iv_val type any
    raising
      zcx_ajson_error.

  methods set_string
    importing
      iv_path type string
      iv_val type clike
    raising
      zcx_ajson_error.

  methods set_integer
    importing
      iv_path type string
      iv_val type i
    raising
      zcx_ajson_error.

  methods set_date
    importing
      iv_path type string
      iv_val type d
    raising
      zcx_ajson_error.

  methods set_null
    importing
      iv_path type string
    raising
      zcx_ajson_error.

  methods delete
    importing
      iv_path type string
    raising
      zcx_ajson_error.

  methods touch_array
    importing
      iv_path type string
      iv_clear type abap_bool default abap_false
    raising
      zcx_ajson_error.

  methods push
    importing
      iv_path type string
      iv_val type any
    raising
      zcx_ajson_error.

  methods set_with_type
    importing
      iv_path type string
      iv_val type any
      iv_type type string
    raising
      zcx_ajson_error.

endinterface.
