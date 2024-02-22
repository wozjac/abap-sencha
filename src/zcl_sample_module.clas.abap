" Dummy module class for the sample
CLASS zcl_sample_module DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      request_daily_report IMPORTING email TYPE string,
      calculate_timezone RETURNING VALUE(result) TYPE string.
ENDCLASS.



CLASS zcl_sample_module IMPLEMENTATION.
  METHOD request_daily_report.
    " ...
    ASSERT email IS NOT INITIAL.
    " ...
  ENDMETHOD.

  METHOD calculate_timezone.
    " ...
    result = 'CET'.
  ENDMETHOD.
ENDCLASS.
