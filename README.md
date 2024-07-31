# ABAP Sencha

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
assert( foo )->equal( 'bar' ). // ABAP Sencha
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

```

## Additional functionality

### Negation with `not`

Most of the check methods can be negated with `not` - in case the method
is not working with negation, an exception will be thrown.

```abap
expect( actual )->not( )->equal_to( 4 ).
the( actual )->should->not( )->be->true( ).
assert( foo )->is->not( )->equal( 'bar' ).
```

### Multiple checks with `and`

### it, describe, when, then, given

## Documentation

The class has documentation available via ABAP Docs.

## Examples

Please check the unit tests in `ZCL_ABAP_SENCHA` for more examples.
Additionally, you can check the sample class ...

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
- <jacekw.dev>
- [X/Twitter](https://x.com/jacekwoz)
- [LinkedIn](https://www.linkedin.com/in/jacek-wznk)
