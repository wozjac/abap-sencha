" Sample class showing some usages of ABAP Sencha.
" The business logic is dummy.
CLASS zcl_sample DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING reporting_system TYPE REF TO zcl_sample_system,
      get_user_header RETURNING VALUE(result) TYPE string,
      calculate_quota RETURNING VALUE(result) TYPE i,
      request_report,
      get_bonus_factor RETURNING VALUE(result) TYPE i.
  PRIVATE SECTION.
    DATA:
    reporting_system TYPE REF TO zcl_sample_system.
ENDCLASS.



CLASS zcl_sample IMPLEMENTATION.

  METHOD constructor.
    me->reporting_system = reporting_system.
  ENDMETHOD.

  METHOD get_user_header.
    SELECT SINGLE FROM usr02
      FIELDS tzone
      WHERE bname = @sy-uname
      INTO @DATA(user_timezone).

    CASE user_timezone.
      WHEN 'CET'.
        result = |[CET user] ${ user_timezone }|.
      WHEN OTHERS.
        result = |[Non-CET] ${ user_timezone }|.
    ENDCASE.

  ENDMETHOD.

  METHOD calculate_quota.
    result = cl_abap_random_int=>create( min = 1 max = 3 )->get_next( ).
  ENDMETHOD.

  METHOD get_bonus_factor.
    result = cl_abap_random_int=>create( min = 1 max = 10 )->get_next( ).
  ENDMETHOD.

  METHOD request_report.
    " ...
    " ...
    reporting_system->request_daily_report( 'reporting@mycompany.internal.com' ).
  ENDMETHOD.

ENDCLASS.
