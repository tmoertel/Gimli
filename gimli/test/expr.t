#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 46;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

#==============================================================================
# tests
#==============================================================================

# comments

evals_ok( "1 # comment", 1 );
evals_ok( "1 /* comment */ + /* comment */ 2", 3 );

# integer literals

evals_same_ok( "1" );
evals_same_ok( "1234" );
evals_same_ok( "-1" );
evals_same_ok( "-1234" );

# string literals

evals_same_ok( '""' );
evals_same_ok( '"foo"' );
evals_same_ok( '"a\nb"' );
evals_ok( '"\65"', '"A"' );    # dec escape
evals_ok( '"\x41"', '"A"' );   # hex escape
evals_ok( '"\o101"', '"A"' );  # oct escape

# bool literals

evals_same_ok( "TRUE" );
evals_same_ok( "FALSE" );
evals_ok( "T", "TRUE" );
evals_ok( "F", "FALSE" );

# equality tests

evals_true_ok( "T==T" );
evals_true_ok( "T==TRUE" );
evals_true_ok( "F == F" );
evals_true_ok( "F == FALSE" );
evals_true_ok( "T != F" );
evals_true_ok( "1 == 1" );
evals_true_ok( "1 != 0" );
evals_true_ok( '"hi" == "hi"' );
evals_true_ok( '"hi" != "foo"' );
evals_false_ok( '"hi" == 1' );
evals_false_ok( 'T == 1' );

# parens

evals_ok( "((((((1))))))", 1 );
evals_ok( "(((T)))", "TRUE" );
evals_true_ok( "(FALSE==F) == T" );
evals_true_ok( "FALSE == (F==T)" );

# arithmetic expressions

evals_ok( "1 + 1", 2 );
evals_ok( "1 - 1", 0 );
evals_ok( "1 * 3", 3 );
evals_ok( "1 * -3", -3 );
evals_ok( "1 + 2 * 3", 7 );
evals_ok( "(1 + 2) * 3", 9 );
evals_ok( "9 / 3", 3 );

# promotion of non-numerics to zero in arithmetic expressions

evals_ok( "1 + T", 1 );
evals_ok( "1 * T", 0 );
evals_ok( '"4" * 1', 0 );

# NAs

evals_same_ok( "NA" );
evals_ok( "(NA)", "NA" );
evals_ok( "NA + 1", "NA" );
evals_ok( "NA == NA", "NA" );
evals_ok( "NA != NA", "NA" );
