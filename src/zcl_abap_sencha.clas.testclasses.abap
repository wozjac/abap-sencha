CLASS ltcl_abap_sencha_calls DEFINITION FOR TESTING
  INHERITING FROM zcl_abap_sencha
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      equal_calls FOR TESTING RAISING cx_static_check,
      expect_subrc_calls FOR TESTING RAISING cx_static_check,
      cover_pattern_calls FOR TESTING RAISING cx_static_check,
      not_cover_pattern_calls FOR TESTING RAISING cx_static_check,
      match_regex_calls FOR TESTING RAISING cx_static_check,
      initial_calls FOR TESTING RAISING cx_static_check,
      not_intitial_calls FOR TESTING RAISING cx_static_check,
      bound_calls FOR TESTING RAISING cx_static_check,
      not_bound_calls FOR TESTING RAISING cx_static_check,
      true_calls FOR TESTING RAISING cx_static_check,
      false_calls FOR TESTING RAISING cx_static_check,
      between_calls FOR TESTING RAISING cx_static_check,
      contained_in_calls FOR TESTING RAISING cx_static_check,
      satisfy_calls FOR TESTING RAISING cx_static_check,
      and_calls FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS lcl_constraint DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_constraint.
ENDCLASS.




" It is not needed to test all data types, as the Expect class does not introduce
" any derivations from the ABAP Unit assert in this area and just passes the actual/expected
" values to the right assert... methods
CLASS ltcl_abap_sencha_calls IMPLEMENTATION.

  METHOD equal_calls.
    given( 'Sample description' ).
    DATA(actual) = 1.

    when( 'Sample description' ).
    then( ).

    expect( actual )->equal_to( 1 ).
    expect( actual )->equals( 1 ).
    expect( actual )->equal( 1 ).
    expect( actual )->equals_to( 1 ).

    value( actual )->should->equal_to( 1 ).
    v( actual )->should->equals( 1 ).
    v( actual )->should->be->equal( 1 ).
    v( actual )->should->equals_to( 1 ).

    expect( actual )->not( )->equal_to( 2 ).
    expect( actual )->not( )->equal( 2 ).
    expect( actual )->not( )->equals_to( 2 ).

    value( actual )->should->not( )->equal_to( 2 ).
    v( actual )->should->not( )->equal( 2 ).
    v( actual )->should->not( )->equals_to( 2 ).
  ENDMETHOD.

  METHOD expect_subrc_calls.
    DATA tab TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2 ##SUBRC_OK.
    expect_subrc( )->equal( 4 ).

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2 ##SUBRC_OK.
    expect_subrc( )->not( )->equal( 0 ).

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2 ##SUBRC_OK.
    subrc( )->should->equal( 4 ).

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2.
    return_code( sy-subrc )->should->not( )->equal( 0 ).

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2.
    expect_return_code( sy-subrc )->should->not( )->equal( 0 ).
  ENDMETHOD.

  METHOD cover_pattern_calls.
    DATA(actual) = 'abcde'.

    expect( actual )->cover_pattern( '*bc*' ).
    expect( actual )->covers_pattern( '+bcd+' ).
    expect( actual )->match_pattern( '*bc*' ).
    expect( actual )->matches_pattern( '+bcd+' ).

    value( actual )->should->cover_pattern( '*bc*' ).
    v( actual )->should->match_pattern( '*bc*' ).

    expect( actual )->not( )->cover_pattern( '*cdd*' ).
    expect( actual )->not( )->covers_pattern( '+cbd+' ).
    expect( actual )->not( )->match_pattern( '*cdd*' ).
    expect( actual )->not( )->matches_pattern( '+cbd+' ).

    v( actual )->should->not( )->cover_pattern( '*cdd*' ).
    v( actual )->should->not( )->match_pattern( '*cdd*' ).
  ENDMETHOD.

  METHOD not_cover_pattern_calls.
    DATA(actual) = 'abcde'.

    expect( actual )->not_cover_pattern( '*cb*' ).
    expect( actual )->not_cover_pattern( '+bb+' ).
    v( actual )->should->not_cover_pattern( '*cb*' ).
    v( actual )->should->not_cover_pattern( '+bb+' ).

    expect( actual )->not( )->not_cover_pattern( '*bc*' ).
    expect( actual )->not( )->not_cover_pattern( '+bcd+' ).
    v( actual )->should->not( )->not_cover_pattern( '*bc*' ).
    v( actual )->should->not( )->not_cover_pattern( '+bcd+' ).
  ENDMETHOD.

  METHOD match_regex_calls.
    DATA(actual) = 'jacek'.

    expect( actual )->match_regex( 'ja\w' ).
    expect( actual )->matches_regex( 'ja\w' ).
    v( actual )->should->match_regex( 'ja\w' ).

    expect( actual )->not( )->match_regex( 'ja\d' ).
    expect( actual )->not( )->matches_regex( 'ja\d' ).
    the( actual )->should->not( )->match_regex( 'ja\d' ).

    " Example of negated match regex: expect( actual )->not( )->match_regex( 'ja\w' ).
  ENDMETHOD.

  METHOD initial_calls.
    DATA actual TYPE i.

    expect( actual )->initial( ).
    v( actual )->should->be->initial( ).

    actual = 2.
    expect( actual )->is->not( )->initial( ).
    the( actual )->should->not( )->be->initial( ).
  ENDMETHOD.

  METHOD not_intitial_calls.
    DATA(actual) = 2.

    expect( actual )->not_initial( ).
    v( actual )->should->be->not_initial( ).
    CLEAR actual.
    expect( actual )->is->not( )->not_initial( ).
    v( actual )->should->not( )->be->not_initial( ).
  ENDMETHOD.

  METHOD bound_calls.
    DATA(actual) = NEW zcl_abap_sencha( ).

    expect( actual )->bound( ).
    value( actual )->should->be->bound( ).
    the( actual )->should->be->bound( ).

    CLEAR actual.
    expect( actual )->not( )->bound( ).
    v( actual )->should->not( )->be->bound( ).
    the( actual )->should->not( )->be->bound( ).
  ENDMETHOD.

  METHOD not_bound_calls.
    DATA actual TYPE REF TO zcl_abap_sencha.

    expect( actual )->not_bound( ).
    v( actual )->should->be->not_bound( ).
    actual = NEW #( ).
    expect( actual )->not( )->not_bound( ).
    the( actual )->should->not( )->be->not_bound( ).
  ENDMETHOD.

  METHOD true_calls.
    DATA(actual) = abap_true.

    expect( actual )->true( ).
    v( actual )->should->be->true( ).
    CLEAR actual.
    expect( actual )->not( )->to->be->true( ).
    v( actual )->should->not( )->be->true( ).
  ENDMETHOD.

  METHOD false_calls.
    DATA(actual) = abap_false.

    expect( actual )->false( ).
    v( actual )->should->be->false( ).
    actual = abap_true.
    expect( actual )->not( )->to->be->false( ).
    v( actual )->should->not( )->be->false( ).
  ENDMETHOD.

  METHOD between_calls.
    DATA(actual) = 5.

    expect( actual )->between( lower = 1 upper = 5 ).

    " NOT is not allowed with between, below would fail the
    " test:  expect( actual )->not( )->between( lower = 1 upper = 6 ).

    value( actual )->should->be->between( lower = 1 upper = 5 ).
  ENDMETHOD.

  METHOD and_calls.
    DATA: BEGIN OF value,
            val1 TYPE string,
            val2 TYPE string,
          END OF value.

    value-val1 = 'a'.
    value-val2 = 'b'.

    expect( value-val1 )->equals( 'a' )->and( value-val2 )->equals( 'b' ).
    expect( value-val1 )->to->equal( 'a' )->and( value-val2 )->not( )->equal( 'c' ).
    expect( value-val1 )->not( )->equal( 's' )->and( value-val2 )->not( )->equal( 'c' ).
    expect( value-val1 )->not( )->equal( 's' )->and( value-val2 )->equal( 'b' ).

    " Examples of fail: expect( value-val1 )->not( )->equal( 'a' )->and( value-val2 )->equal( 'b' ).
    " or: expect( value-val1 )->not( )->equal( 's' )->and( value-val2 )->equal( 'k' ).

    value( value-val1 )->should->equals( 'a' )->and( value-val2 )->should->equals( 'b' ).
    v( value-val1 )->should->equal( 'a' )->and( value-val2 )->should->should->not( )->equal( 'c' ).
    v( value-val1 )->should->not( )->equal( 's' )->and( value-val2 )->should->not( )->equal( 'c' ).
    v( value-val1 )->should->not( )->equal( 's' )->and( value-val2 )->should->equal( 'b' ).
  ENDMETHOD.

  METHOD contained_in_calls.
    TYPES: BEGIN OF sample,
             val1 TYPE i,
             val2 TYPE string,
           END OF sample.

    DATA:
      packed_value TYPE p VALUE 5,
      int_value    TYPE i VALUE 7,
      float_value  TYPE f VALUE 11,
      char_value   TYPE c LENGTH 2 VALUE 'ab',
      string_value TYPE string VALUE 'ab',
      string_table TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      struct_table TYPE STANDARD TABLE OF sample WITH EMPTY KEY.

    string_table = VALUE #( ( |abc| ) ( |def| ) ).
    struct_table = VALUE #( ( val1 = 1 val2 = 'abc' ) ( val1 = 2 val2 = 'def' ) ).

    expect( packed_value )->contained_in( value = 1 upper = 7 ).
    expect( int_value )->contained_in( value = 1 upper = 7 ).
    expect( float_value )->contained_in( value = 4 upper = 12 ).

    value( packed_value )->should->be->contained_in( value = 1 upper = 7 ).
    v( int_value )->should->be->contained_in( value = 1 upper = 7 ).
    v( float_value )->should->be->contained_in( value = 4 upper = 12 ).

    expect( char_value )->contained_in( 'dabcd' ).
    expect( char_value )->not( )->contained_in( 'dbacd' ).
    expect( string_value )->not( )->contained_in( 'dbacd' ).

    v( char_value )->should->be->contained_in( 'dabcd' ).
    v( char_value )->should->not( )->be->contained_in( 'dbacd' ).

    expect( |def| )->contained_in( string_table ).
    expect( |qwe| )->not( )->contained_in( string_table ).

    v( |def| )->should->be->contained_in( string_table ).
    v( |qwe| )->should->not( )->be->contained_in( string_table ).

    expect( VALUE sample( val1 = 1 val2 = 'abc' ) )->contained_in( struct_table ).
    expect( VALUE sample( val1 = 1 val2 = 'def' ) )->not( )->contained_in( struct_table ).

    v( VALUE sample( val1 = 1 val2 = 'abc' ) )->should->be->contained_in( struct_table ).
    v( VALUE sample( val1 = 1 val2 = 'def' ) )->should->not( )->be->contained_in( struct_table ).

  ENDMETHOD.

  METHOD satisfy_calls.
    DATA(constraint) = NEW lcl_constraint( ).
    expect( |hello| )->to->satisfy( constraint ).
    expect( |hello| )->satisfies( constraint ).
    value( |hello| )->should->satisfy( constraint ).
  ENDMETHOD.

ENDCLASS.



CLASS lcl_constraint IMPLEMENTATION.

  METHOD if_constraint~get_description.
    result = VALUE #( ( |My custom constraint| ) ).
  ENDMETHOD.

  METHOD if_constraint~is_valid.
    FIELD-SYMBOLS <fs> TYPE string.
    ASSIGN data_object TO <fs>.

    IF <fs> = 'hello'.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
