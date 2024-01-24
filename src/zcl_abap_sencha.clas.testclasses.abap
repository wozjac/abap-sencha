CLASS ltcl_abap_sencha DEFINITION FOR TESTING
  INHERITING FROM zcl_abap_sencha
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      expect_equal FOR TESTING RAISING cx_static_check,
      should_equal FOR TESTING RAISING cx_static_check,
      assert_equal FOR TESTING RAISING cx_static_check,
      mixed_equal FOR TESTING RAISING cx_static_check,

      expect_subrc_ FOR TESTING RAISING cx_static_check,
      should_subrc FOR TESTING RAISING cx_static_check,
      assert_subrc_ FOR TESTING RAISING cx_static_check,
      mixed_subrc FOR TESTING RAISING cx_static_check,

      expect_cover_pattern FOR TESTING RAISING cx_static_check,
      should_cover_pattern FOR TESTING RAISING cx_static_check,
      assert_cover_pattern FOR TESTING RAISING cx_static_check,
      mixed_cover_pattern FOR TESTING RAISING cx_static_check,

      expect_not_cover_pattern FOR TESTING RAISING cx_static_check,
      should_not_cover_pattern FOR TESTING RAISING cx_static_check,
      assert_not_cover_pattern FOR TESTING RAISING cx_static_check,
      mixed_not_cover_pattern FOR TESTING RAISING cx_static_check,

      expect_match_regex FOR TESTING RAISING cx_static_check,
      should_match_regex FOR TESTING RAISING cx_static_check,
      assert_match_regex FOR TESTING RAISING cx_static_check,
      mixed_match_regex FOR TESTING RAISING cx_static_check,

      expect_initial FOR TESTING RAISING cx_static_check,
      should_initial FOR TESTING RAISING cx_static_check,
      assert_initial FOR TESTING RAISING cx_static_check,
      mixed_initial FOR TESTING RAISING cx_static_check,

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
CLASS ltcl_abap_sencha IMPLEMENTATION.

  METHOD expect_equal.
    given( 'Sample description' ).
    DATA(actual) = 1.

    when( 'Sample description' ).
    then( ).

    expect( actual )->equal_to( expected = 1 ).

    actual = 2.
    expect( actual )->equals( 2 ).

    actual = 3.
    expect( actual )->equal( expected = 3 ).

    actual = 4.
    expect( actual )->equals_to( 4 ).

    actual = 5.
    expect( actual )->not( )->equal_to( 4 ).

    actual = 6.
    expect( actual )->not( )->equal( 5 ).

    actual = 7.
    expect( actual )->not( )->equals_to( 6 ).
  ENDMETHOD.

  METHOD assert_equal.
    DATA(actual) = 1.

    assert( )->equal( actual = actual expected = 1 ).

    actual = 2.
    assert( )->equals_to( actual = actual expected = 2 ).

    actual = 3.
    assert( )->equal_to( actual = actual expected = 3 ).

    actual = 4.
    assert( )->equals( actual = actual expected = 4 ).

    actual = 5.
    assert( )->not( )->equal( actual = actual expected = 4 ).

    actual = 6.
    assert( )->not( )->equal( actual = actual expected = 5 ).

    actual = 7.
    assert( )->not( )->equal( actual = actual expected = 6 ).

    actual = 8.
    assert( actual )->equals_to( 8 ).

    actual = 9.
    assert( actual )->equal_to( 9 ).

    actual = 10.
    assert( actual )->not( )->equal( 9 ).

  ENDMETHOD.

  METHOD should_equal.
    DATA(actual) = 1.

    value( actual )->should->equal_to( 1 ).

    actual = 2.
    v( actual )->should->equals( 2 ).

    actual = 3.
    the( actual )->should->be->equal( 3 ).

    actual = 4.
    v( actual )->should->equals_to( 4 ).

    actual = 5.
    value( actual )->should->not( )->equal_to( 4 ).

    actual = 6.
    v( actual )->should->not( )->equal( 5 ).

    actual = 7.
    v( actual )->should->not( )->equals_to( 6 ).
  ENDMETHOD.

  METHOD mixed_equal.
    DATA(actual) = 1.

    expect( actual )->equal_to( expected = 1 ).

    actual = 2.
    expect( actual )->equals( 2 ).

    actual = 3.
    expect( actual )->equal( expected = 3 ).

    actual = 4.
    expect( actual )->equals_to( 4 ).

    actual = 5.
    value( actual )->should->not( )->equal_to( 4 ).

    actual = 6.
    v( actual )->should->not( )->equal( 5 ).

    actual = 7.
    v( actual )->should->not( )->equals_to( 6 ).

    actual = 8.
    assert( actual )->equals_to( 8 ).

    actual = 9.
    assert( actual )->equal_to( 9 ).

    actual = 10.
    assert( actual )->not( )->equal( 9 ).
  ENDMETHOD.

  METHOD expect_subrc_.
    DATA tab TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2 ##SUBRC_OK.
    expect_subrc( )->equal( 4 ).

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2 ##SUBRC_OK.
    expect_subrc( )->not( )->equal( 0 ).

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2.
    expect_return_code( sy-subrc )->should->not( )->equal( 0 ).
  ENDMETHOD.

  METHOD should_subrc.
    DATA tab TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2 ##SUBRC_OK.
    subrc( )->should->equal( 4 ).

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2.
    return_code( sy-subrc )->should->not( )->equal( 0 ).
  ENDMETHOD.

  METHOD assert_subrc_.
    DATA tab TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2 ##SUBRC_OK.
    assert_subrc( )->equal( 4 ).

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2 ##SUBRC_OK.
    assert_return_code( )->equals( 4 ).

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2 ##SUBRC_OK.
    assert_subrc( )->not( )->equals_to( 0 ).

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2 ##SUBRC_OK.
    assert_return_code( )->not( )->equal_to( 0 ).
  ENDMETHOD.

  METHOD mixed_subrc.
    DATA tab TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2 ##SUBRC_OK.
    expect_subrc( )->equal( 4 ).

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2 ##SUBRC_OK.
    expect_subrc( )->not( )->equal( 0 ).

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2 ##SUBRC_OK.
    subrc( )->should->equal( 4 ).

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2.
    return_code( sy-subrc )->should->not( )->equal( 0 ).

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2 ##SUBRC_OK.
    assert_return_code( )->equals( 4 ).

    READ TABLE tab TRANSPORTING NO FIELDS INDEX 2 ##SUBRC_OK.
    assert_subrc( )->not( )->equals_to( 0 ).
  ENDMETHOD.

  METHOD expect_cover_pattern.
    DATA(actual) = 'abcde'.
    expect( actual )->cover_pattern( '*bc*' ).

    actual = 'ertal'.
    expect( actual )->covers_pattern( '+rt++' ).

    actual = 'qwerty'.
    expect( actual )->match_pattern( '*we*' ).

    actual = 'zxcv'.
    expect( actual )->matches_pattern( '+xc+' ).

    actual = 'dfgh'.
    expect( actual )->not( )->cover_pattern( '*dfgg*' ).

    actual = 'porta'.
    expect( actual )->not( )->covers_pattern( '+krt+' ).

    actual = 'asdfg'.
    expect( actual )->not( )->match_pattern( '*bsd*' ).

    actual = 'poiu'.
    expect( actual )->not( )->matches_pattern( '+io+' ).
  ENDMETHOD.

  METHOD should_cover_pattern.
    DATA(actual) = 'abcde'.
    v( actual )->should->cover_pattern( '*bc*' ).

    actual = 'ertal'.
    value( actual )->should->covers_pattern( '+rt++' ).

    actual = 'qwerty'.
    the( actual )->should->match_pattern( '*we*' ).

    actual = 'zxcv'.
    v( actual )->should->matches_pattern( '+xc+' ).

    actual = 'dfgh'.
    v( actual )->should->not( )->cover_pattern( '*dfgg*' ).

    actual = 'porta'.
    value( actual )->should->not( )->covers_pattern( '+krt+' ).

    actual = 'asdfg'.
    v( actual )->should->not( )->match_pattern( '*bsd*' ).

    actual = 'poiu'.
    v( actual )->should->not( )->matches_pattern( '+io+' ).
  ENDMETHOD.

  METHOD assert_cover_pattern.
    DATA(actual) = 'abcde'.
    assert( )->cover_pattern( actual = actual pattern = '*bc*' ).

    actual = 'ertal'.
    assert( actual )->covers_pattern( '+rt++' ).

    actual = 'qwerty'.
    assert( actual )->match_pattern( '*we*' ).

    actual = 'zxcv'.
    assert( )->matches_pattern( actual = actual pattern = '+xc+' ).

    actual = 'dfgh'.
    assert( actual )->not( )->cover_pattern( '*dfgg*' ).

    actual = 'porta'.
    assert( )->not( )->covers_pattern( actual = actual pattern = '+krt+' ).

    actual = 'asdfg'.
    assert( actual )->not( )->match_pattern( '*bsd*' ).

    actual = 'poiu'.
    assert( )->not( )->matches_pattern( actual = actual pattern = '+io+' ).
  ENDMETHOD.

  METHOD mixed_cover_pattern.
    DATA(actual) = 'abcde'.
    expect( actual )->cover_pattern( '*bc*' ).

    actual = 'asdfg'.
    expect( actual )->not( )->match_pattern( '*bsd*' ).

    actual = 'zxcv'.
    v( actual )->should->matches_pattern( '+xc+' ).

    actual = 'dfgh'.
    v( actual )->should->not( )->cover_pattern( '*dfgg*' ).

    actual = 'zxcv'.
    assert( )->matches_pattern( actual = actual pattern = '+xc+' ).

    actual = 'dfgh'.
    assert( actual )->not( )->cover_pattern( '*dfgg*' ).
  ENDMETHOD.

  METHOD expect_not_cover_pattern.
    DATA(actual) = 'abcde'.
    expect( actual )->not_cover_pattern( '*cb*' ).

    actual = 'ghjkl'.
    expect( actual )->not_cover_pattern( '+hh+' ).

    actual = 'erty'.
    expect( actual )->not( )->not_cover_pattern( '*rt*' ).

    actual = 'asdfg'.
    expect( actual )->not( )->not_cover_pattern( '+sdf+' ).
  ENDMETHOD.

  METHOD should_not_cover_pattern.
    DATA(actual) = 'abcde'.
    v( actual )->should->not_cover_pattern( '*cb*' ).

    actual = 'ghjkl'.
    v( actual )->should->not_cover_pattern( '+bb+' ).

    actual = 'erty'.
    v( actual )->not( )->not_cover_pattern( '*rt*' ).

    actual = 'asdfg'.
    v( actual )->not( )->not_cover_pattern( '+sdf+' ).
  ENDMETHOD.

  METHOD assert_not_cover_pattern.
    DATA(actual) = 'abcde'.
    assert( actual )->not_cover_pattern( '*cb*' ).

    actual = 'ghjkl'.
    assert( )->not_cover_pattern( actual = actual pattern = '+hh+' ).

    actual = 'erty'.
    assert( actual )->not( )->not_cover_pattern( '*rt*' ).

    actual = 'asdfg'.
    assert( )->not( )->not_cover_pattern( actual = actual pattern = '+sdf+' ).
  ENDMETHOD.

  METHOD mixed_not_cover_pattern.
    DATA(actual) = 'abcde'.
    expect( actual )->not_cover_pattern( '*cb*' ).

    actual = 'erty'.
    expect( actual )->not( )->not_cover_pattern( '*rt*' ).

    actual = 'ghjkl'.
    v( actual )->should->not_cover_pattern( '+bb+' ).

    actual = 'erty'.
    v( actual )->not( )->not_cover_pattern( '*rt*' ).

    actual = 'erty'.
    assert( actual )->not( )->not_cover_pattern( '*rt*' ).

    actual = 'asdfg'.
    assert( )->not( )->not_cover_pattern( actual = actual pattern = '+sdf+' ).
  ENDMETHOD.

  METHOD expect_match_regex.
    DATA(actual) = 'jacek'.
    expect( actual )->match_regex( 'ja\w' ).

    actual = 'emilia'.
    expect( actual )->matches_regex( 'em\w' ).

    actual = 'emilia'.
    expect( actual )->not( )->match_regex( 'ja\d' ).

    actual = 'mamba'.
    expect( actual )->not( )->matches_regex( 'ma\d' ).

    " Example of negated match regex: expect( actual )->not( )->match_regex( 'ja\w' ).
  ENDMETHOD.

  METHOD should_match_regex.
    DATA(actual) = 'jacek'.
    v( actual )->should->match_regex( 'ja\w' ).

    actual = 'lapis'.
    the( actual )->should->not( )->match_regex( 'ja\d' ).
  ENDMETHOD.

  METHOD assert_match_regex.
    DATA(actual) = 'jacek'.
    assert( actual )->match_regex( 'ja\w' ).

    actual = 'emilia'.
    assert( )->matches_regex( actual = actual regex = 'em\w' ).

    actual = 'emilia'.
    assert( actual )->not( )->match_regex( 'ja\d' ).

    actual = 'mamba'.
    assert( )->not( )->matches_regex( actual = actual regex = 'ma\d' ).
  ENDMETHOD.

  METHOD mixed_match_regex.
    DATA(actual) = 'jacek'.
    expect( actual )->match_regex( 'ja\w' ).

    actual = 'emilia'.
    expect( actual )->not( )->match_regex( 'ja\d' ).

    actual = 'jacek'.
    v( actual )->should->match_regex( 'ja\w' ).

    actual = 'lapis'.
    the( actual )->should->not( )->match_regex( 'ja\d' ).

    actual = 'emilia'.
    assert( )->matches_regex( actual = actual regex = 'em\w' ).

    actual = 'emilia'.
    assert( actual )->not( )->match_regex( 'ja\d' ).
  ENDMETHOD.

  METHOD expect_initial.
    DATA actual TYPE i.
    expect( actual )->initial( ).

    actual = 2.
    expect( actual )->is->not( )->initial( ).
  ENDMETHOD.

  METHOD should_initial.
    DATA actual TYPE i.
    v( actual )->should->be->initial( ).

    actual = 2.
    the( actual )->should->not( )->be->initial( ).
  ENDMETHOD.

  METHOD assert_initial.
    DATA actual TYPE i.
    assert( actual )->initial( ).

    actual = 2.
    assert( )->is->not( )->initial( actual ).
  ENDMETHOD.

  METHOD mixed_initial.
    DATA actual TYPE i.
    expect( actual )->initial( ).

    actual = 2.
    expect( actual )->is->not( )->initial( ).

    CLEAR actual.
    v( actual )->should->be->initial( ).

    actual = 2.
    the( actual )->should->not( )->be->initial( ).

    CLEAR actual.
    assert( actual )->initial( ).

    actual = 2.
    assert( )->is->not( )->initial( actual ).
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
