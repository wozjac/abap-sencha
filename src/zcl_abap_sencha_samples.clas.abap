" Sample class showing some usages of ABAP Sencha.
" The business logic is dummy.
CLASS zcl_abap_sencha_samples DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING some_module TYPE REF TO zcl_abap_sencha_sample_module,
      get_user_header RETURNING VALUE(result) TYPE string,
      calculate_quota RETURNING VALUE(result) TYPE i,
      request_report,
      get_bonus_factor RETURNING VALUE(result) TYPE i.
  PRIVATE SECTION.
    DATA some_module TYPE REF TO zcl_abap_sencha_sample_module.
ENDCLASS.



CLASS zcl_abap_sencha_samples IMPLEMENTATION.

  METHOD constructor.
    me->some_module = some_module.
  ENDMETHOD.

  METHOD get_user_header.
    DATA(timezone) = some_module->calculate_timezone( ).

    CASE timezone.
      WHEN 'CET'.
        result = |[CET user] { sy-uname }|.
      WHEN OTHERS.
        result = |[Non-CET] { sy-uname }|.
    ENDCASE.
  ENDMETHOD.

  METHOD calculate_quota.
    TRY.
        result = cl_abap_random_int=>create( min = 1 max = 3 )->get_next( ).
      CATCH cx_abap_random.
        " ...
    ENDTRY.
  ENDMETHOD.

  METHOD get_bonus_factor.
    TRY.
        result = cl_abap_random_int=>create( min = 1 max = 10 )->get_next( ).
      CATCH cx_abap_random.
        " ...
    ENDTRY.
  ENDMETHOD.

  METHOD request_report.
    " ...
    " ...
    some_module->request_daily_report( 'reporting@mycompany.internal.com' ).
  ENDMETHOD.

ENDCLASS.
