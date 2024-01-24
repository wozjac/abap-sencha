"! <p class="shorttext synchronized" lang="en">ABAP Sencha</p>
CLASS zcl_abap_sencha DEFINITION PUBLIC CREATE PROTECTED.
  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    "! Initializes language chains.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS:
      " Main methods

      "! <p class="shorttext synchronized" lang="en">An entry method for checks using "expect" style</p>
      "!
      "! <p>This method sets the stage for checking provided values with one of the check methods.
      "! The parameters are passed to the respective CL_ABAP_UNIT_ASSERT methods, wrapped
      "! by check methods like bound, equals etc.</p>
      "!
      "! @parameter actual | <p class="shorttext synchronized" lang="en">Actual value</p>
      "! @parameter message | <p class="shorttext synchronized" lang="en">Message (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter level | <p class="shorttext synchronized" lang="en">Severity level (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter quit | <p class="shorttext synchronized" lang="en">Control flow (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      expect IMPORTING actual        TYPE any OPTIONAL
                       message       TYPE string OPTIONAL
                       level         TYPE int1 DEFAULT if_abap_unit_constant=>severity-medium
                       quit          TYPE int1 DEFAULT if_abap_unit_constant=>quit-test
                         PREFERRED PARAMETER actual
             RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">An entry method for checks using "should" style</p>
      "!
      "! <p>This method sets the stage for checking provided values with one of the check methods.
      "! The parameters are passed to the respective CL_ABAP_UNIT_ASSERT methods, wrapped
      "! by check methods like bound, equals etc.</p>
      "!
      "! @parameter actual | <p class="shorttext synchronized" lang="en">Actual value</p>
      "! @parameter message | <p class="shorttext synchronized" lang="en">Message (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter level | <p class="shorttext synchronized" lang="en">Severity level (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter quit | <p class="shorttext synchronized" lang="en">Control flow (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      value IMPORTING actual        TYPE any OPTIONAL
                      message       TYPE string OPTIONAL
                      level         TYPE int1 DEFAULT if_abap_unit_constant=>severity-medium
                      quit          TYPE int1 DEFAULT if_abap_unit_constant=>quit-test
                        PREFERRED PARAMETER actual
            RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,


      "! <p class="shorttext synchronized" lang="en">An entry method for checking a return code</p>
      "!
      "! @parameter subrc | <p class="shorttext synchronized" lang="en">SY-SUBRC value</p>
      "! @parameter message | <p class="shorttext synchronized" lang="en">Message (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter level | <p class="shorttext synchronized" lang="en">Severity level (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter quit | <p class="shorttext synchronized" lang="en">Control flow (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      expect_subrc IMPORTING subrc         TYPE sysubrc DEFAULT sy-subrc
                             message       TYPE string OPTIONAL
                             level         TYPE int1 DEFAULT if_abap_unit_constant=>severity-medium
                             quit          TYPE int1 DEFAULT if_abap_unit_constant=>quit-test
                               PREFERRED PARAMETER subrc
                   RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">An entry method for the subrc check with "should" style</p>
      "!
      "! @parameter subrc | <p class="shorttext synchronized" lang="en">SY-SUBRC value</p>
      "! @parameter message | <p class="shorttext synchronized" lang="en">Message (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter level | <p class="shorttext synchronized" lang="en">Severity level (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter quit | <p class="shorttext synchronized" lang="en">Control flow (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      subrc IMPORTING subrc         TYPE sysubrc DEFAULT sy-subrc
                      message       TYPE string OPTIONAL
                      level         TYPE int1 DEFAULT if_abap_unit_constant=>severity-medium
                      quit          TYPE int1 DEFAULT if_abap_unit_constant=>quit-test
                        PREFERRED PARAMETER subrc
            RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">An entry method for the return code check for "should" style</p>
      "!
      "! @parameter return_code | <p class="shorttext synchronized" lang="en">Return code value</p>
      "! @parameter message | <p class="shorttext synchronized" lang="en">Message (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter level | <p class="shorttext synchronized" lang="en">Severity level (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter quit | <p class="shorttext synchronized" lang="en">Control flow (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      return_code IMPORTING return_code   TYPE sysubrc DEFAULT sy-subrc
                            message       TYPE string OPTIONAL
                            level         TYPE int1 DEFAULT if_abap_unit_constant=>severity-medium
                            quit          TYPE int1 DEFAULT if_abap_unit_constant=>quit-test
                              PREFERRED PARAMETER return_code
                  RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">An entry method for checking a return code</p>
      "!
      "! <p>It is the same as expect_subrc, just a different name.</p>
      "!
      "! @parameter return_code | <p class="shorttext synchronized" lang="en">Return code value, default SY-SUBRC</p>
      "! @parameter message | <p class="shorttext synchronized" lang="en">Message (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter level | <p class="shorttext synchronized" lang="en">Severity level (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter quit | <p class="shorttext synchronized" lang="en">Control flow (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      expect_return_code IMPORTING return_code   TYPE sysubrc DEFAULT sy-subrc
                                   message       TYPE string OPTIONAL
                                   level         TYPE int1 DEFAULT if_abap_unit_constant=>severity-medium
                                   quit          TYPE int1 DEFAULT if_abap_unit_constant=>quit-test
                                     PREFERRED PARAMETER return_code
                         RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">Used for chained checks</p>
      "!
      "! <p>expect( value-val1 )->equals( 'a' )->and( value-val2 )->equals( 'b' ).</p>
      "!
      "! @parameter actual | <p class="shorttext synchronized" lang="en">Actual value</p>
      "! @parameter message | <p class="shorttext synchronized" lang="en">Message (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter level | <p class="shorttext synchronized" lang="en">Severity level (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter quit | <p class="shorttext synchronized" lang="en">Control flow (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      and IMPORTING actual        TYPE any OPTIONAL
                    message       TYPE string OPTIONAL
                    level         TYPE int1 DEFAULT if_abap_unit_constant=>severity-medium
                    quit          TYPE int1 DEFAULT if_abap_unit_constant=>quit-test
                      PREFERRED PARAMETER actual
          RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,


      "! <p class="shorttext synchronized" lang="en">Negates ONLY the next check</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      not RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,


      "! <p class="shorttext synchronized" lang="en">Can be used to add a description for the 'given' section</p>
      "!
      "! @parameter description | <p class="shorttext synchronized" lang="en">Textual description</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      given IMPORTING description   TYPE string OPTIONAL
            RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,


      "! <p class="shorttext synchronized" lang="en">Can be used to add a description for the 'when' section</p>
      "!
      "! @parameter description | <p class="shorttext synchronized" lang="en">Textual description</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      when IMPORTING description   TYPE string OPTIONAL
           RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,


      "! <p class="shorttext synchronized" lang="en">Can be used to add a description for the 'then' section</p>
      "!
      "! @parameter description | <p class="shorttext synchronized" lang="en">Textual description</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      then IMPORTING description   TYPE string OPTIONAL
           RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,

      " Assert wrapper methods

      "! <p class="shorttext synchronized" lang="en">Equality check, a wrapper for ASSERT_EQUALS</p>
      "!
      "! @parameter actual | <p class="shorttext synchronized" lang="en">Actual value</p>
      "! <p>Use here in the 'assert' style, overwrites the actual value if set in 'expect' and 'should' style.</p>
      "! @parameter expected | <p class="shorttext synchronized" lang="en">Expected value</p>
      "! @parameter float_tolerance | <p class="shorttext synchronized" lang="en">Float value tolerance</p>
      "! @parameter ignore_hash_sequence | <p class="shorttext synchronized" lang="en">Ignore sequence in
      "! hash tables</p>
      "! @parameter message | <p class="shorttext synchronized" lang="en">Message (see CL_ABAP_UNIT_ASSERT)</p>
      "! <p>Use here in the 'assert' style, overwrites the actual value if set in 'expect' and 'should' style.</p>
      "! @parameter level | <p class="shorttext synchronized" lang="en">Severity level (see CL_ABAP_UNIT_ASSERT)</p>
      "! <p>Use here in the 'assert' style, overwrites the actual value if set in 'expect' and 'should' style.</p>
      "! @parameter quit | <p class="shorttext synchronized" lang="en">Control flow (see CL_ABAP_UNIT_ASSERT)</p>
      "! <p>Use here in the 'assert' style, overwrites the actual value if set in 'expect' and 'should' style.</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      equal IMPORTING actual               TYPE any OPTIONAL
                      expected             TYPE any
                      float_tolerance      TYPE f OPTIONAL
                      ignore_hash_sequence TYPE abap_bool DEFAULT abap_false
                      message              TYPE string OPTIONAL
                      level                TYPE int1 DEFAULT if_abap_unit_constant=>severity-medium
                      quit                 TYPE int1 DEFAULT if_abap_unit_constant=>quit-test
            RETURNING VALUE(result)        TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">Is the value contained in</p>
      "! Works with
      "! - internal tables (uses ASSERT_TABLE_CONTAINS)
      "! - character values (uses COVER_PATTERN, *value*)
      "! - numbers (uses BETWEEN)
      "!
      "! @parameter value | <p class="shorttext synchronized" lang="en">An internal table,
      "! a characters sequence or a number</p>
      "! If used for number in range check, the 'value' is treated as the lower boundary
      "! @parameter upper | <p class="shorttext synchronized" lang="en">Upper boundary value for BETWEEN checks</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      contained_in IMPORTING value         TYPE any
                             upper         TYPE numeric OPTIONAL
                   RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,


      "! <p class="shorttext synchronized" lang="en">Pattern matching check, a wrapper for ASSERT_CHAR_CP</p>
      "!
      "! @parameter pattern | <p class="shorttext synchronized" lang="en">Pattern</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      cover_pattern IMPORTING actual        TYPE any OPTIONAL
                              pattern       TYPE csequence
                              message       TYPE string OPTIONAL
                              level         TYPE int1 DEFAULT if_abap_unit_constant=>severity-medium
                              quit          TYPE int1 DEFAULT if_abap_unit_constant=>quit-test
                    RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">Pattern matching check, a wrapper for ASSERT_CHAR_NP</p>
      "!
      "! @parameter pattern | <p class="shorttext synchronized" lang="en">Pattern</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      not_cover_pattern IMPORTING pattern       TYPE csequence
                        RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">Regex matching check, a wrapper for ASSERT_TEXT_MATCHES</p>
      "! When negated, uses 'contains' built-in function.
      "!
      "! @parameter regex | <p class="shorttext synchronized" lang="en">Regex</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      match_regex IMPORTING regex         TYPE csequence
                  RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,


      "! <p class="shorttext synchronized" lang="en">Number in range check, a wrapper for ASSERT_NUMBER_BETWEEN</p>
      "!
      "! @parameter lower | <p class="shorttext synchronized" lang="en">Lower number</p>
      "! @parameter upper | <p class="shorttext synchronized" lang="en">Upper number</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      between IMPORTING lower         TYPE numeric
                        upper         TYPE numeric
              RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">Initial value check, a wrapper for ASSERT_INITIAL</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      initial RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,


      "! <p class="shorttext synchronized" lang="en">Non-initial value check, a wrapper for ASSERT_NOT_INITIAL</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      not_initial RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,


      "! <p class="shorttext synchronized" lang="en">Bound/not bound object check</p>
      "!
      "! Uses ASSERT_BOUND / ASSERT_NOT_BOUND (if negated with "NOT")
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      bound RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,


      "! <p class="shorttext synchronized" lang="en">Bound/not bound check</p>
      "!
      "! Uses ASSERT_BOUND / ASSERT_NOT_BOUND (if negated with "NOT")
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      not_bound RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,


      "! <p class="shorttext synchronized" lang="en">True value check, a wrapper for ASSERT_TRUE</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      true RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,


      "! <p class="shorttext synchronized" lang="en">False value check, a wrapper for ASSERT_FALSE</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      false RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,


      "! <p class="shorttext synchronized" lang="en">Check for a custom condition, a wrapper for ASSERT_THAT</p>
      "!
      "! @parameter actual_as_text | <p class="shorttext synchronized" lang="en">Description used in the
      "! alert message</p>
      "! @parameter constraint | <p class="shorttext synchronized" lang="en">Constraint object</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
      satisfy IMPORTING actual_as_text TYPE csequence OPTIONAL
                        constraint     TYPE REF TO if_constraint
              RETURNING VALUE(result)  TYPE REF TO zcl_abap_sencha,


      "! <p class="shorttext synchronized" lang="en">Failure, a wrapper for FAIL</p>
      "!
      "! @parameter detail | <p class="shorttext synchronized" lang="en">Description</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      fail IMPORTING detail        TYPE csequence OPTIONAL
           RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,

      " Variations
      " Different names acting as wrappers for assert wrapper methods

      "! <p class="shorttext synchronized" lang="en">Equality check, calls EQUAL</p>
      "!
      "! @parameter actual | <p class="shorttext synchronized" lang="en">Actual value</p>
      "! <p>Use here in the 'assert' style, overwrites the actual value if set in 'expect' and 'should' style.</p>
      "! @parameter expected | <p class="shorttext synchronized" lang="en">Expected value</p>
      "! @parameter float_tolerance | <p class="shorttext synchronized" lang="en">Float value tolerance</p>
      "! @parameter ignore_hash_sequence | <p class="shorttext synchronized" lang="en">Ignore sequence in
      "! hash tables</p>
      "! @parameter message | <p class="shorttext synchronized" lang="en">Message (see CL_ABAP_UNIT_ASSERT)</p>
      "! <p>Use here in the 'assert' style, overwrites the actual value if set in 'expect' and 'should' style.</p>
      "! @parameter level | <p class="shorttext synchronized" lang="en">Severity level (see CL_ABAP_UNIT_ASSERT)</p>
      "! <p>Use here in the 'assert' style, overwrites the actual value if set in 'expect' and 'should' style.</p>
      "! @parameter quit | <p class="shorttext synchronized" lang="en">Control flow (see CL_ABAP_UNIT_ASSERT)</p>
      "! <p>Use here in the 'assert' style, overwrites the actual value if set in 'expect' and 'should' style.</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      equals IMPORTING actual               TYPE any OPTIONAL
                       expected             TYPE any
                       float_tolerance      TYPE f OPTIONAL
                       ignore_hash_sequence TYPE abap_bool DEFAULT abap_false
                       message              TYPE string OPTIONAL
                       level                TYPE int1 DEFAULT if_abap_unit_constant=>severity-medium
                       quit                 TYPE int1 DEFAULT if_abap_unit_constant=>quit-test
             RETURNING VALUE(result)        TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">Equality check, calls EQUAL</p>
      "!
      "! @parameter actual | <p class="shorttext synchronized" lang="en">Actual value</p>
      "! <p>Use here in the 'assert' style, overwrites the actual value if set in 'expect' and 'should' style.</p>
      "! @parameter expected | <p class="shorttext synchronized" lang="en">Expected value</p>
      "! @parameter float_tolerance | <p class="shorttext synchronized" lang="en">Float value tolerance</p>
      "! @parameter ignore_hash_sequence | <p class="shorttext synchronized" lang="en">Ignore sequence in
      "! hash tables</p>
      "! @parameter message | <p class="shorttext synchronized" lang="en">Message (see CL_ABAP_UNIT_ASSERT)</p>
      "! <p>Use here in the 'assert' style, overwrites the actual value if set in 'expect' and 'should' style.</p>
      "! @parameter level | <p class="shorttext synchronized" lang="en">Severity level (see CL_ABAP_UNIT_ASSERT)</p>
      "! <p>Use here in the 'assert' style, overwrites the actual value if set in 'expect' and 'should' style.</p>
      "! @parameter quit | <p class="shorttext synchronized" lang="en">Control flow (see CL_ABAP_UNIT_ASSERT)</p>
      "! <p>Use here in the 'assert' style, overwrites the actual value if set in 'expect' and 'should' style.</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      equal_to IMPORTING actual               TYPE any OPTIONAL
                         expected             TYPE any
                         float_tolerance      TYPE f OPTIONAL
                         ignore_hash_sequence TYPE abap_bool DEFAULT abap_false
                         message              TYPE string OPTIONAL
                         level                TYPE int1 DEFAULT if_abap_unit_constant=>severity-medium
                         quit                 TYPE int1 DEFAULT if_abap_unit_constant=>quit-test
               RETURNING VALUE(result)        TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">Equality check, calls EQUAL</p>
      "!
      "! @parameter actual | <p class="shorttext synchronized" lang="en">Actual value</p>
      "! <p>Use here in the 'assert' style, overwrites the actual value if set in 'expect' and 'should' style.</p>
      "! @parameter expected | <p class="shorttext synchronized" lang="en">Expected value</p>
      "! @parameter float_tolerance | <p class="shorttext synchronized" lang="en">Float value tolerance</p>
      "! @parameter ignore_hash_sequence | <p class="shorttext synchronized" lang="en">Ignore sequence
      "! in hash tables</p>
      "! @parameter message | <p class="shorttext synchronized" lang="en">Message (see CL_ABAP_UNIT_ASSERT)</p>
      "! <p>Use here in the 'assert' style, overwrites the actual value if set in 'expect' and 'should' style.</p>
      "! @parameter level | <p class="shorttext synchronized" lang="en">Severity level (see CL_ABAP_UNIT_ASSERT)</p>
      "! <p>Use here in the 'assert' style, overwrites the actual value if set in 'expect' and 'should' style.</p>
      "! @parameter quit | <p class="shorttext synchronized" lang="en">Control flow (see CL_ABAP_UNIT_ASSERT)</p>
      "! <p>Use here in the 'assert' style, overwrites the actual value if set in 'expect' and 'should' style.</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      equals_to IMPORTING actual               TYPE any OPTIONAL
                          expected             TYPE any
                          float_tolerance      TYPE f OPTIONAL
                          ignore_hash_sequence TYPE abap_bool DEFAULT abap_false
                          message              TYPE string OPTIONAL
                          level                TYPE int1 DEFAULT if_abap_unit_constant=>severity-medium
                          quit                 TYPE int1 DEFAULT if_abap_unit_constant=>quit-test
                RETURNING VALUE(result)        TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">Regex matching check, calls MATCH_REGEX</p>
      "! When negated, uses 'contains' built-in function.
      "!
      "! @parameter regex | <p class="shorttext synchronized" lang="en">Regex</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      matches_regex IMPORTING regex         TYPE csequence
                    RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">Pattern matching check, calls COVER_PATTERN</p>
      "!
      "! @parameter pattern | <p class="shorttext synchronized" lang="en">Pattern</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      covers_pattern IMPORTING actual        TYPE any OPTIONAL
                               pattern       TYPE any
                               message       TYPE string OPTIONAL
                               level         TYPE int1 DEFAULT if_abap_unit_constant=>severity-medium
                               quit          TYPE int1 DEFAULT if_abap_unit_constant=>quit-test
                     RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">Pattern matching check, calls COVER_PATTERN</p>
      "!
      "! @parameter pattern | <p class="shorttext synchronized" lang="en">Pattern</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      match_pattern IMPORTING actual        TYPE any OPTIONAL
                              pattern       TYPE any
                              message       TYPE string OPTIONAL
                              level         TYPE int1 DEFAULT if_abap_unit_constant=>severity-medium
                              quit          TYPE int1 DEFAULT if_abap_unit_constant=>quit-test
                    RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">Pattern matching check, calls COVER_PATTERN</p>
      "!
      "! @parameter pattern | <p class="shorttext synchronized" lang="en">Pattern</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      matches_pattern IMPORTING actual        TYPE any OPTIONAL
                                pattern       TYPE any
                                message       TYPE string OPTIONAL
                                level         TYPE int1 DEFAULT if_abap_unit_constant=>severity-medium
                                quit          TYPE int1 DEFAULT if_abap_unit_constant=>quit-test
                      RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">Check for a custom condition, calls SATISFY</p>
      "!
      "! @parameter actual_as_text | <p class="shorttext synchronized" lang="en">Description used in
      "! the alert message</p>
      "! @parameter constraint | <p class="shorttext synchronized" lang="en">Constraint object</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
      satisfies IMPORTING actual_as_text TYPE csequence OPTIONAL
                          constraint     TYPE REF TO if_constraint
                RETURNING VALUE(result)  TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">A wrapper for "value" method</p>
      "!
      "! This method sets the stage for checking provided values with one of the check methods.
      "! The parameters are passed to the respective CL_ABAP_UNIT_ASSERT methods, wrapped
      "! by check methods like bound, equals etc.
      "!
      "! @parameter actual | <p class="shorttext synchronized" lang="en">Actual value</p>
      "! @parameter message | <p class="shorttext synchronized" lang="en">Message (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter level | <p class="shorttext synchronized" lang="en">Severity level (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter quit | <p class="shorttext synchronized" lang="en">Control flow (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      v IMPORTING actual        TYPE any OPTIONAL
                  message       TYPE string OPTIONAL
                  level         TYPE int1 DEFAULT if_abap_unit_constant=>severity-medium
                  quit          TYPE int1 DEFAULT if_abap_unit_constant=>quit-test
                    PREFERRED PARAMETER actual
        RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">A wrapper for "value" method</p>
      "!
      "! This method sets the stage for checking provided values with one of the check methods.
      "! The parameters are passed to the respective CL_ABAP_UNIT_ASSERT methods, wrapped
      "! by check methods like bound, equals etc.
      "!
      "! @parameter actual | <p class="shorttext synchronized" lang="en">Actual value</p>
      "! @parameter message | <p class="shorttext synchronized" lang="en">Message (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter level | <p class="shorttext synchronized" lang="en">Severity level (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter quit | <p class="shorttext synchronized" lang="en">Control flow (see CL_ABAP_UNIT_ASSERT)</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      the IMPORTING actual        TYPE any OPTIONAL
                    message       TYPE string OPTIONAL
                    level         TYPE int1 DEFAULT if_abap_unit_constant=>severity-medium
                    quit          TYPE int1 DEFAULT if_abap_unit_constant=>quit-test
                      PREFERRED PARAMETER actual
          RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha,


      "! <p class="shorttext synchronized" lang="en">Skip the test execution</p>
      "!
      "! @parameter details | <p class="shorttext synchronized" lang="en">Description</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">The current object instance</p>
      skip IMPORTING details       TYPE csequence OPTIONAL
           RETURNING VALUE(result) TYPE REF TO zcl_abap_sencha.

    DATA:
      " Language chains

      "! <p class="shorttext synchronized" lang="en">Language chain</p>
      is     TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">Language chain</p>
      be     TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">Language chain</p>
      been   TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">Language chain</p>
      to     TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">Language chain</p>
      does   TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">'assert' style chain</p>
      assert TYPE REF TO zcl_abap_sencha,

      "! <p class="shorttext synchronized" lang="en">'should' style chain</p>
      should TYPE REF TO zcl_abap_sencha.

  PRIVATE SECTION.
    DATA:
      "! <p class="shorttext synchronized" lang="en">Actual value passed to ASSERT... methods</p>
      actual     TYPE REF TO data,

      "! <p class="shorttext synchronized" lang="en">The copy of SUBRC</p>
      subrc_copy TYPE sysubrc,

      "! <p class="shorttext synchronized" lang="en">Message passed to ASSERT... methods</p>
      message    TYPE string,

      "! <p class="shorttext synchronized" lang="en">Negation indicator</p>
      negation   TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">Severity level passed to ASSERT... methods</p>
      level      TYPE int1,

      "! <p class="shorttext synchronized" lang="en">Quit behavior passed to ASSERT... methods</p>
      quit       TYPE int1.
ENDCLASS.



CLASS zcl_abap_sencha IMPLEMENTATION.
  METHOD constructor.
    is = me.
    be = me.
    been = me.
    to = me.
    does = me.
    should = me.
    assert = me.
  ENDMETHOD.

  METHOD expect.
    me->actual = REF #( actual ).
    me->message = message.
    me->level = level.
    me->quit = quit.
    result = me.
  ENDMETHOD.

  METHOD value.
    me->actual = REF #( actual ).
    me->message = message.
    me->level = level.
    me->quit = quit.
    result = me.
  ENDMETHOD.

  METHOD expect_subrc.
    me->subrc_copy = subrc.
    me->actual = REF #( me->subrc_copy ).
    me->message = message.
    me->level = level.
    me->quit = quit.
    result = me.
  ENDMETHOD.

  METHOD expect_return_code.
    result = expect_subrc(
      subrc   = return_code
      message = message
      level   = level
      quit    = quit ).
  ENDMETHOD.

  METHOD subrc.
    me->subrc_copy = subrc.
    me->actual = REF #( me->subrc_copy ).
    me->message = message.
    me->level = level.
    me->quit = quit.
    result = me.
  ENDMETHOD.

  METHOD return_code.
    result = subrc(
      subrc   = return_code
      message = message
      level   = level
      quit    = quit ).
  ENDMETHOD.

  METHOD and.
    IF actual IS SUPPLIED.
      me->actual = REF #( actual ).
      me->message = message.
      me->level = level.
      me->quit = quit.
    ENDIF.

    result = me.
  ENDMETHOD.

  METHOD not.
    me->negation = abap_true.
    result = me.
  ENDMETHOD.

  METHOD given.
    result = me.
  ENDMETHOD.

  METHOD then.
    result = me.
  ENDMETHOD.

  METHOD when.
    result = me.
  ENDMETHOD.



  METHOD equal.
    IF actual IS SUPPLIED.
      me->actual = REF #( actual ).
    ENDIF.

    IF message IS SUPPLIED.
      me->message = message.
    ENDIF.

    IF level IS SUPPLIED.
      me->level = level.
    ENDIF.

    IF quit IS SUPPLIED.
      me->quit = quit.
    ENDIF.

    FIELD-SYMBOLS <actual> TYPE any.
    ASSIGN me->actual->* TO <actual>.

    IF me->negation = abap_true.
      cl_abap_unit_assert=>assert_differs(
        msg = me->message
        level = me->level
        quit = me->quit
        tol = float_tolerance
        act = <actual>
        exp = expected ).
    ELSE.
      cl_abap_unit_assert=>assert_equals(
        msg = me->message
        level = me->level
        quit = me->quit
        tol = float_tolerance
        ignore_hash_sequence = ignore_hash_sequence
        act = <actual>
        exp = expected ).
    ENDIF.

    CLEAR me->negation.
    result = me.
  ENDMETHOD.

  METHOD contained_in.
    DATA pattern TYPE string.
    FIELD-SYMBOLS <actual> TYPE any.
    DATA(value_description) = cl_abap_typedescr=>describe_by_data( value ).

    CASE value_description->type_kind.
      WHEN cl_abap_typedescr=>typekind_int
        OR cl_abap_typedescr=>typekind_int8
        OR cl_abap_typedescr=>typekind_packed
        OR cl_abap_typedescr=>typekind_decfloat
        OR cl_abap_typedescr=>typekind_decfloat16
        OR cl_abap_typedescr=>typekind_decfloat34
        OR cl_abap_typedescr=>typekind_float.

        result = between( lower = value upper = upper ).

      WHEN cl_abap_typedescr=>typekind_char
        OR cl_abap_typedescr=>typekind_numeric
        OR cl_abap_typedescr=>typekind_string.

        ASSIGN me->actual->* TO <actual>.
        pattern = '*' && <actual> && '*'.
        me->actual = REF #( value ).
        result = cover_pattern( pattern ).

      WHEN cl_abap_typedescr=>typekind_table.
        ASSIGN me->actual->* TO <actual>.

        IF me->negation = abap_true.
          cl_abap_unit_assert=>assert_table_not_contains(
            line = <actual>
            table = value
            msg = me->message
            level = me->level
            quit = me->quit ).
        ELSE.
          cl_abap_unit_assert=>assert_table_contains(
            line = <actual>
            table = value
            msg = me->message
            level = me->level
            quit = me->quit ).
        ENDIF.

        CLEAR me->negation.
        result = me.

    ENDCASE.
  ENDMETHOD.

  METHOD between.
    FIELD-SYMBOLS <actual> TYPE any.

    IF me->negation = abap_true.
      CLEAR me->negation.
      cl_abap_unit_assert=>fail( 'NOT method not supported to use with BETWEEN' ).
    ENDIF.

    ASSIGN me->actual->* TO <actual>.

    cl_abap_unit_assert=>assert_number_between(
      msg = me->message
      level = me->level
      quit = me->quit
      number = <actual>
      lower = lower
      upper = upper ).

    CLEAR me->negation.
    result = me.
  ENDMETHOD.

  METHOD bound.
    FIELD-SYMBOLS <actual> TYPE any.
    ASSIGN me->actual->* TO <actual>.

    IF me->negation = abap_true.
      cl_abap_unit_assert=>assert_not_bound(
        msg = me->message
        level = me->level
        quit = me->quit
        act = <actual> ).
    ELSE.
      cl_abap_unit_assert=>assert_bound(
        msg = me->message
        level = me->level
        quit = me->quit
        act = <actual> ).
    ENDIF.

    CLEAR me->negation.
    result = me.
  ENDMETHOD.

  METHOD not_bound.
    FIELD-SYMBOLS <actual> TYPE any.
    ASSIGN me->actual->* TO <actual>.

    IF me->negation = abap_true.
      cl_abap_unit_assert=>assert_bound(
        msg = me->message
        level = me->level
        quit = me->quit
        act = <actual> ).
    ELSE.
      cl_abap_unit_assert=>assert_not_bound(
        msg = me->message
        level = me->level
        quit = me->quit
        act = <actual> ).
    ENDIF.

    CLEAR me->negation.
    result = me.
  ENDMETHOD.

  METHOD initial.
    FIELD-SYMBOLS <actual> TYPE any.
    ASSIGN me->actual->* TO <actual>.

    IF me->negation = abap_true.
      cl_abap_unit_assert=>assert_not_initial(
        msg = me->message
        level = me->level
        quit = me->quit
        act = <actual> ).
    ELSE.
      cl_abap_unit_assert=>assert_initial(
        msg = me->message
        level = me->level
        quit = me->quit
        act = <actual> ).
    ENDIF.

    CLEAR me->negation.
    result = me.
  ENDMETHOD.

  METHOD not_initial.
    FIELD-SYMBOLS <actual> TYPE any.
    ASSIGN me->actual->* TO <actual>.

    IF me->negation = abap_true.
      cl_abap_unit_assert=>assert_initial(
        msg = me->message
        level = me->level
        quit = me->quit
        act = <actual> ).
    ELSE.
      cl_abap_unit_assert=>assert_not_initial(
        msg = me->message
        level = me->level
        quit = me->quit
        act = <actual> ).
    ENDIF.

    CLEAR me->negation.
    result = me.
  ENDMETHOD.

  METHOD true.
    FIELD-SYMBOLS <actual> TYPE any.
    ASSIGN me->actual->* TO <actual>.

    IF me->negation = abap_true.
      cl_abap_unit_assert=>assert_false(
        msg = me->message
        level = me->level
        quit = me->quit
        act = <actual> ).
    ELSE.
      cl_abap_unit_assert=>assert_true(
        msg = me->message
        level = me->level
        quit = me->quit
        act = <actual> ).
    ENDIF.

    CLEAR me->negation.
    result = me.
  ENDMETHOD.

  METHOD false.
    FIELD-SYMBOLS <actual> TYPE any.
    ASSIGN me->actual->* TO <actual>.

    IF me->negation = abap_true.
      cl_abap_unit_assert=>assert_true(
        msg = me->message
        level = me->level
        quit = me->quit
        act = <actual> ).
    ELSE.
      cl_abap_unit_assert=>assert_false(
        msg = me->message
        level = me->level
        quit = me->quit
        act = <actual> ).
    ENDIF.

    CLEAR me->negation.
    result = me.
  ENDMETHOD.

  METHOD cover_pattern.
    IF actual IS SUPPLIED.
      me->actual = REF #( actual ).
    ENDIF.

    IF message IS SUPPLIED.
      me->message = message.
    ENDIF.

    IF level IS SUPPLIED.
      me->level = level.
    ENDIF.

    IF quit IS SUPPLIED.
      me->quit = quit.
    ENDIF.

    FIELD-SYMBOLS <actual> TYPE any.
    ASSIGN me->actual->* TO <actual>.

    IF me->negation = abap_true.
      cl_abap_unit_assert=>assert_char_np(
        msg = me->message
        level = me->level
        quit = me->quit
        act = <actual>
        exp = pattern ).
    ELSE.
      cl_abap_unit_assert=>assert_char_cp(
        msg = me->message
        level = me->level
        quit = me->quit
        act = <actual>
        exp = pattern ).
    ENDIF.

    CLEAR me->negation.
    result = me.
  ENDMETHOD.

  METHOD not_cover_pattern.
    FIELD-SYMBOLS <actual> TYPE any.
    ASSIGN me->actual->* TO <actual>.

    IF me->negation = abap_true.
      cl_abap_unit_assert=>assert_char_cp(
        msg = me->message
        level = me->level
        quit = me->quit
        act = <actual>
        exp = pattern ).
    ELSE.
      cl_abap_unit_assert=>assert_char_np(
        msg = me->message
        level = me->level
        quit = me->quit
        act = <actual>
        exp = pattern ).
    ENDIF.

    CLEAR me->negation.
    result = me.
  ENDMETHOD.

  METHOD match_regex.
    FIELD-SYMBOLS <actual> TYPE any.
    ASSIGN me->actual->* TO <actual>.

    IF me->negation = abap_true.
      IF contains( val = <actual> regex = regex ).
        cl_abap_unit_assert=>fail(
          msg = |Text '{ <actual> }' should not match regex '{ regex }'|
          level = me->level
          quit = me->quit ).
      ENDIF.
    ELSE.
      cl_abap_unit_assert=>assert_text_matches(
        msg = me->message
        level = me->level
        quit = me->quit
        text = <actual>
        pattern = regex ).
    ENDIF.

    CLEAR me->negation.
    result = me.
  ENDMETHOD.

  METHOD satisfy.
    FIELD-SYMBOLS <actual> TYPE any.

    IF me->negation = abap_true.
      CLEAR me->negation.
      cl_abap_unit_assert=>fail( 'NOT method not supported to use with THAT' ).
    ENDIF.

    ASSIGN me->actual->* TO <actual>.

    cl_abap_unit_assert=>assert_that(
      act = <actual>
      act_as_text = actual_as_text
      exp = constraint
      msg = me->message
      level = me->level
      quit = me->quit ).

    result = me.

  ENDMETHOD.

  METHOD fail.
    cl_abap_unit_assert=>fail(
      msg    = message
      level  = level
      quit   = quit
      detail = detail ).
  ENDMETHOD.

  METHOD skip.
    cl_abap_unit_assert=>skip(
      msg    = message
      detail = details ).
  ENDMETHOD.



  METHOD equals_to.
    IF actual IS SUPPLIED. " assert style
      result = equal(
        actual = actual
        expected = expected
        float_tolerance = float_tolerance
        ignore_hash_sequence = ignore_hash_sequence
        message = message
        level = level
        quit = quit ).
    ELSE. " should, expect style
      result = equal(
        expected = expected
        float_tolerance = float_tolerance
        ignore_hash_sequence = ignore_hash_sequence ).
    ENDIF.
  ENDMETHOD.

  METHOD equals.
    IF actual IS SUPPLIED. " assert style
      result = equal(
        actual = actual
        expected = expected
        float_tolerance = float_tolerance
        ignore_hash_sequence = ignore_hash_sequence
        message = message
        level = level
        quit = quit ).
    ELSE. " should, expect style
      result = equal(
        expected = expected
        float_tolerance = float_tolerance
        ignore_hash_sequence = ignore_hash_sequence ).
    ENDIF.
  ENDMETHOD.

  METHOD equal_to.
    IF actual IS SUPPLIED. " assert style
      result = equal(
        actual = actual
        expected = expected
        float_tolerance = float_tolerance
        ignore_hash_sequence = ignore_hash_sequence
        message = message
        level = level
        quit = quit ).
    ELSE. " should, expect style
      result = equal(
        expected = expected
        float_tolerance = float_tolerance
        ignore_hash_sequence = ignore_hash_sequence ).
    ENDIF.
  ENDMETHOD.

  METHOD covers_pattern.
    result = cover_pattern( pattern ).
  ENDMETHOD.

  METHOD match_pattern.
    result = cover_pattern( pattern ).
  ENDMETHOD.

  METHOD matches_pattern.
    result = cover_pattern( pattern ).
  ENDMETHOD.

  METHOD matches_regex.
    result = match_regex( regex ).
  ENDMETHOD.

  METHOD satisfies.
    result = satisfy(
      actual_as_text = actual_as_text
      constraint = constraint ).
  ENDMETHOD.

  METHOD v.
    result = value(
      actual  = actual
      message = message
      level   = level
      quit    = quit ).
  ENDMETHOD.

  METHOD the.
    result = value(
      actual  = actual
      message = message
      level   = level
      quit    = quit ).
  ENDMETHOD.

ENDCLASS.
