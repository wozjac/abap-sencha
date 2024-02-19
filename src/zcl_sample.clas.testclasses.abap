CLASS ltcl_sample DEFINITION FINAL FOR TESTING
  INHERITING FROM zcl_abap_sencha
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ENUM mock_user_timezone_case,
        cet,
        non_cet,
      END OF ENUM mock_user_timezone_case.

    CLASS-DATA:
      sql_environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown,

      user_header_for_cet FOR TESTING,
      user_header_for_non_cet FOR TESTING,
      bonus_factor_in_range FOR TESTING,
      allowed_quota FOR TESTING,
      daily_report_request FOR TESTING,

      user_timezone_mocked_for IMPORTING case TYPE mock_user_timezone_case,
      request_report_configured.

    DATA:
      cut                   TYPE REF TO zcl_sample,
      reporting_system_mock TYPE REF TO zcl_sample_system.
ENDCLASS.


CLASS ltcl_sample IMPLEMENTATION.

  METHOD setup.
    reporting_system_mock = CAST zcl_sample_system( get_mock_for( 'ZCL_SAMPLE_SYSTEM' ) ).
    cut = NEW #( reporting_system_mock ).
  ENDMETHOD.

  METHOD teardown.
    sql_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD class_setup.
    sql_environment = cl_osql_test_environment=>create( VALUE #( ( 'USR02' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    sql_environment->destroy( ).
  ENDMETHOD.

  METHOD user_timezone_mocked_for.
    DATA usr02 TYPE STANDARD TABLE OF usr02 WITH DEFAULT KEY.

    usr02 = VALUE #(
      ( bname = sy-uname
        tzone = SWITCH #( case WHEN cet THEN 'CET' ELSE 'EST' ) ) ).

    sql_environment->insert_test_data( usr02 ).
  ENDMETHOD.

  METHOD user_header_for_cet.
    " verbose comments in the given-when-then
    given( 'the user uses the CET timezone' ).
    user_timezone_mocked_for( cet ).

    when( 'we request the header' ).
    DATA(header) = cut->get_user_header( ).

    then( 'it should be prepended by CET phrase' ).
    the( header )->should->cover_pattern( '*CET user*' ).
  ENDMETHOD.

  METHOD user_header_for_non_cet.
    given( ).
    user_timezone_mocked_for( non_cet ).

    when( ).
    DATA(header) = cut->get_user_header( ).

    then( ).
    the( header )->should->cover_pattern( '*Non-CET*' ).
  ENDMETHOD.

  METHOD bonus_factor_in_range.
    " it( 'returns bonus factor in the range of 1 and 10
    expect( cut->get_bonus_factor( ) )->is->between( lower = 1 upper = 10 ).
  ENDMETHOD.

  METHOD allowed_quota.
    DATA allowed_quotas TYPE STANDARD TABLE OF i.
    allowed_quotas = VALUE #( ( 1 ) ( 2 ) ( 3 ) ).

    DATA(actual_quota) = cut->calculate_quota( ).

    expect( actual_quota )->is->contained_in( allowed_quotas ).
  ENDMETHOD.

  METHOD daily_report_request.
    given( ).
    request_report_configured( ).

    when( ).
    cut->request_report( ).

    then( ).
    verify_expectations( reporting_system_mock ).
  ENDMETHOD.

  METHOD request_report_configured.
    configure_call( reporting_system_mock
                  )->ignore_parameter( 'EMAIL'
                  )->and_expect(
                  )->is_called_once( ).

    reporting_system_mock->request_daily_report( 'some_email' ).
  ENDMETHOD.

ENDCLASS.
