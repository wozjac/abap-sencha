CLASS ltcl_sample DEFINITION FINAL FOR TESTING
  INHERITING FROM zcl_abap_sencha
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PROTECTED SECTION.
    METHODS:
      given REDEFINITION,
      when REDEFINITION,
      then REDEFINITION.

  PRIVATE SECTION.
    TYPES mock_timezone_case TYPE i.

    CONSTANTS:
      cet     TYPE mock_timezone_case VALUE 0,
      non_cet TYPE mock_timezone_case VALUE 1.

*    in newer ABAP you can use enums
*    TYPES:
*      BEGIN OF ENUM mock_timezone_case,
*        cet,
*        non_cet,
*      END OF ENUM mock_timezone_case.

    METHODS:
      setup,

      user_header_for_cet FOR TESTING,
      user_header_for_non_cet FOR TESTING,
      bonus_factor_in_range FOR TESTING,
      allowed_quota FOR TESTING,
      daily_report_request FOR TESTING,

      timezone_mocked_for IMPORTING case TYPE mock_timezone_case,
      request_report_configured.

    DATA:
      cut              TYPE REF TO zcl_sample,
      some_module_mock TYPE REF TO zcl_sample_module.
ENDCLASS.


CLASS ltcl_sample IMPLEMENTATION.

  METHOD setup.
    some_module_mock = CAST zcl_sample_module( get_mock_for( 'ZCL_SAMPLE_MODULE' ) ).
    cut = NEW #( some_module_mock ).
  ENDMETHOD.

  METHOD given.
    super->given( ).

    " see method daily_report_request
    IF description CP '*request*report*configured*'.
      request_report_configured( ).
      " ...
    ENDIF.
  ENDMETHOD.

  METHOD when.
    super->when( ).

    " see method daily_report_request
    IF description CP '*report*requested*'.
      cut->request_report( ).
    ENDIF.
  ENDMETHOD.

  METHOD then.
    super->then( ).

    " see method daily_report_request
    IF description CP '*module*receives*request*'.
      verify_expectations( some_module_mock ).
    ENDIF.
  ENDMETHOD.

  METHOD timezone_mocked_for.
    configure_call( some_module_mock
                  )->returning( SWITCH #( case WHEN cet THEN 'CET' ELSE 'EST' )
                  )->and_expect(
                  )->is_called_once( ).

    some_module_mock->calculate_timezone( ).
  ENDMETHOD.

  METHOD user_header_for_cet.
    " verbose comments in the given-when-then
    given( 'the user uses the CET timezone' ).
    timezone_mocked_for( cet ).

    when( 'we request the header' ).
    DATA(header) = cut->get_user_header( ).

    then( 'it should be prepended by CET phrase' ).
    the( header )->should->cover_pattern( '*CET user*' ).
  ENDMETHOD.

  METHOD user_header_for_non_cet.
    describe( 'User header for non CET users' ).

    given( ).
    timezone_mocked_for( non_cet ).

    when( ).
    DATA(header) = cut->get_user_header( ).

    then( ).
    the( header )->should->cover_pattern( '*Non-CET*' ).
  ENDMETHOD.

  METHOD bonus_factor_in_range.
    it( 'returns bonus factor in the range of 1 and 10' ).
    expect( cut->get_bonus_factor( ) )->is->between( lower = 1 upper = 10 ).
  ENDMETHOD.

  METHOD allowed_quota.
    DATA allowed_quotas TYPE STANDARD TABLE OF i WITH NON-UNIQUE KEY table_line.
    allowed_quotas = VALUE #( ( 1 ) ( 2 ) ( 3 ) ).

    DATA(actual_quota) = cut->calculate_quota( ).

    expect( actual_quota )->is->contained_in( allowed_quotas ).
  ENDMETHOD.

  METHOD daily_report_request.
    " simplified BDD approach
    given( 'request report is configured [...]' ).
    when( 'report is requested' ).
    then( 'module*receives*request' ).
  ENDMETHOD.

  METHOD request_report_configured.
    configure_call( some_module_mock
                  )->ignore_parameter( 'EMAIL'
                  )->and_expect(
                  )->is_called_once( ).

    some_module_mock->request_daily_report( 'some_email' ).
  ENDMETHOD.

ENDCLASS.
