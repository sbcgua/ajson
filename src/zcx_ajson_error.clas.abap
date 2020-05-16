class ZCX_AJSON_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  types:
    ty_rc type c length 4 .

  data RC type TY_RC read-only .
  data MESSAGE type STRING read-only .
  data LOCATION type STRING read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !RC type TY_RC optional
      !MESSAGE type STRING optional
      !LOCATION type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_AJSON_ERROR IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->RC = RC .
me->MESSAGE = MESSAGE .
me->LOCATION = LOCATION .
endmethod.
ENDCLASS.
