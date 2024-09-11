# ABAP Sencha 🍵

BDD / TDD assertion library for ABAP, inspired by [Chai.js](https://www.chaijs.com).

Chai.js allows you to work with 3 different flavors:

![chai js flavors](img/chai%20overview.png)

The goal of ABAP Sencha is to provide similar functionality in ABAP on top of the existing ABAP Unit framework
to leverage its tooling and IDE support.

In a nutshell:

```js
foo.should.equal('bar'); // chai.js
the( foo )->should->equal( 'bar' ). //ABAP Sencha

expect(foo).to.equal('bar'); // chai.js
expect( foo )->to->equal( 'bar' ). // ABAP Sencha

assert.equal(foo, 'bar'); // chai.js
assert->equal( actual = foo expected = 'bar' ) // ABAP Sencha
assert( foo )->equals( 'bar' ). // ABAP Sencha
```

## Overview

ABAP Sencha tries to bring the style known from chai.js into ABAP. Obviously not everything,
as it is not JavaScript. The main goals are:

- Enable the usage of all three styles: `should`, `expect`, and `assert`.
- Enable the usage of language chains with a fluent interface.
- Incorporate relevant functions from the chai.js API into ABAP if applicable.
- Introduce ABAP-specific methods to enhance the readability and convenience of tests.

Each of the style is introduced by a dedicated method, followed by
a language chain(s) ending with a check method like `equals` - which are
basically wrappers over `cl_abap_unit_assert` methods. Optionally, a negation
`not` can be used, as well as `and` method for chained, multiple checks.

## State

The current version of the project is 1.0.0-beta.

## But why?

I prefer `chai.js` and find tests written in the `expect` or `should` style to be more
readable compared to other forms. In the JavaScript world, `chai` is always my
preferred choice as it offers a good balance between readability and ease of use.
If you prefer tests to be fully described using natural language, you can check
out the BDD library for ABAP - [Cacamber](https://github.com/dominikpanzer/cacamber-BDD-for-ABAP).

## Styles

### `expect` style

This style is introduced by the method `expect`.

```abap
expect( actual )->equals( 2 ).
expect( actual )->is->equal_to( 2 ).
expect( actual )->not( )->equal_to( 4 ).
expect( char_value )->has->length_of( 3 ).
expect( 1 )->to->be->one_of( some_values ).
expect( actual )->to->be->true( ).
expect( |def| )->contained_in( some_string_table ).
* and more
```

### `should` style

In ABAP you can't use the original `chai.js` approach like `someObject.should...`.
Additional "entry" methods are introduced - `value`/`v`/`the`/`value_of` after
which you can continue to use `should` style.

```abap
the( actual )should->equal( 2 ).
value( actual )->should->be->equal_to( 2 ).
value_of( actual )->should->not( )->equal_to( 4 ).
v( char_value )->should->have->length_of( 3 ).
value( 1 )->should->be->one_of( some_values ).
the( actual )->should->be->true( ).
the( |def| )->should->be->contained_in( some_string_table ).
* and more
```

### `assert` style

`CL_ABAP_UNIT_ASSERT=>...` is the assert style already available in ABAP Unit,
but ABAP Sencha brings the assert style from `chai.js`:

```abap
assert( )->equal( actual = act expected = 1 ).
assert( actual )->equals_to( 8 ).
```

## Additional functionality

## Language chains properties

You can use language chainable properties to improve readability:

- is
- be
- been
- to
- does
- has
- have
- that

```abap
expect( actual )->does-not( )->equal( 4 ).
the( flag )->should->not( )->be->true( ).
```

### Negation with `not( )`

Most of the check methods can be negated with `not` - in case the method
is not working with negation, an exception will be thrown.

```abap
expect( actual )->not( )->equal_to( 4 ).
the( actual )->should->not( )->be->true( ).
assert( foo )->is->not( )->equal( 'bar' ).
```

### Method variations

Some methods have the same functionality exposed via different names,
such as `equal`, `equals`, `equal_to`, `match_pattern`, `matches_pattern`,
and so on. You can use whichever name feels more readable to you.

### Multiple checks with `and`

You can chain related checks with `and` if you prefer:

```abap
expect( bonus-periodic )->is->true( )->and( bonus-amount )->equals( 70 ).
value( bonus-code )->should->equals( 'B' )->and( bonus-amount )->should->equals( 70 ).
assert( bonus-periodic )->is-true( )->and( bonus-amount )->equals( 70 ).
```

### it, describe, when, then, given

ABAP Sencha introduces additional methods from the JavaScript world. These methods
can be used to provide descriptions, which can be helpful due to the limited
length of names in ABAP.

Note: maybe in future those methods provides additional functionality,
there are some ideas.

```abap
given( 'the user uses the CET timezone' ).
timezone_mocked_for( cet ).

when( 'we request the header' ).
DATA(header) = cut->get_user_header( ).

then( 'it should be prepended by CET phrase' ).
the( header )->should->cover_pattern( '*CET user*' ).
```

They can be also used for making tests readable using a
simple approach:

```abap
" redefined methods implemented
METHOD given.
  super->given( ).

  IF description CP '*request*report*configured*'.
    configure_request_report( ).
      " ...
  ENDIF.
ENDMETHOD.

METHOD given.
  super->given( ).

  " see method daily_report_request
  IF description CP '*request*report*configured*'.
    configure_request_report( ).
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
...
" test method
METHOD daily_report_request.
  given( 'request report is configured' ).
  when( 'report is requested' ).
  then( 'module receives request' ).
ENDMETHOD.
```

You can refer to the class `ZCL_ABAP_SENCHA_SAMPLES` for the code samples.

### chai-inspired methods

Aside from the ABAP Unit check methods, there are additional ones inspired by `chai.js`:

- `length_of` for checking the length of internal tables or character-based values
- `one_of` for checking if a value is a member of an internal table
- `contained_in` for checking if a value is contained in a string, number or an internal table

### Additional methods for test doubles

There are handy methods that wrap some functionalities related to test doubles:

- `mock`, `get_mock_for`, `get_test_double_for`, `create_test_double`: wrappers
  for `cl_abap_testdouble=>create`
- `configure_call`: a wrapper for `cl_abap_testdouble=>configure_call`
- `verify_expectations`, `verify`: wrappers for `cl_abap_testdouble=>verify_expectations`

## Usage

Inherit your test class from ZCL_ABAP_SENCHA to gain access to
all the methods and chaining words.

## Documentation

The class has documentation available via ABAP Docs.
The exported, online version is available on <https://wozjac.github.io/abap-sencha/>.

## Examples

Please check the unit tests in `ZCL_ABAP_SENCHA` for usage examples.
Additionally, you can check the sample class `ZCL_ABAP_SENCHA_SAMPLES`.

## Covered CL_ABAP_UNIT_ASSERT methods

NOT - means the method might be negated, like not->cover_pattern("abba")

| CL_ABAP_UNIT_ASSERT                              | ABAP Sencha                           |
| ------------------------------------------------ | ------------------------------------- |
| ASSERT_BOUND, ASSERT_NOT_BOUND                   | BOUND (NOT), NOT_BOUND                |
| ASSERT_CHAR_CP, ASSERT_CHAR_NP                   | (NOT) COVER_PATTERN                   |
| ASSERT_DIFFERS                                   | NOT->EQUAL                            |
| ASSERT_EQUALS                                    | EQUAL                                 |
| ASSERT_EQUALS_FLOAT                              | use EQUAL + float tolerance parameter |
| ASSERT_FALSE                                     | NOT->TRUE, FALSE                      |
| ASSERT_INITIAL, ASSERT_NOT_INITIAL               | (NOT) INITITAL                        |
| ASSERT_NUMBER_BETWEEN                            | BETWEEN                               |
| ASSERT_RETURN_CODE, ASSERT_SUBRC                 | EXPECT_SUBRC, EXPECT_RETURN_CODE      |
| ASSERT_TABLE_CONTAINS, ASSERT_TABLE_NOT_CONTAINS | (NOT) CONTAINED_IN                    |
| ASSERT_TEXT_MATCHES                              | MATCH_REGEX                           |
| ASSERT_THAT                                      | SATISFY                               |
| ASSERT_TRUE                                      | NOT->FALSE, TRUE                      |
| ASSUME_TRUE, ASSUME_FALSE                        | ASSUME->TRUE/FALSE                    |
| ASSUME_THAT                                      | ASSUME->SATISFY                       |
| FAIL                                             | FAIL                                  |
| SKIP                                             | SKIP                                  |

## Compatibility

The source code syntax is checked using [abapLint](https://abaplint.org) `v740sp08`
`Cloud` version.

## Technical design

ABAP Sencha functionality is delivered by one big class with internal redundancy.
This is intended design, as its goal is to avoid additional calls in the ABAP Unit test
framework stack traces as much as possible (which would be present if the functionality would
be split into multiple, smaller pieces of code).

## Similar projects

- [Cacamber](https://github.com/dominikpanzer/cacamber-BDD-for-ABAP)
- [assert](https://github.com/abapify/assert)

## License

This plugin is licensed under the MIT license.

## Author

Feel free to contact me:

- <wozjac@zoho.com>
- <https://jacekw.dev>
- [X/Twitter](https://x.com/jacekwoz)
- [LinkedIn](https://www.linkedin.com/in/jacek-wznk)
