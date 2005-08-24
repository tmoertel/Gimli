#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 73;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

#==============================================================================
# tests
#==============================================================================

# comments

evals_ok( "1 # comment", 1 );
evals_same_ok( '"not a # comment"' );

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

evals_same_ok( "T" );
evals_same_ok( "F" );
evals_ok( "TRUE" , "T" );
evals_ok( "FALSE", "F" );

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
evals_true_ok( 'T == 1' );

# comparisions

evals_false_ok( '0 <  0' );
evals_true_ok ( '0 <  1' );
evals_true_ok ( '0 <= 0' );
evals_true_ok ( '0 <= 1' );
evals_false_ok( '0 >  0' );
evals_false_ok( '0 >  1' );
evals_true_ok ( '0 >= 0' );
evals_false_ok( '0 >= 1' );
evals_false_ok( '1 <  0' );
evals_false_ok( '1 <  1' );
evals_false_ok( '1 <= 0' );
evals_true_ok ( '1 <= 1' );
evals_true_ok ( '1 >  0' );
evals_false_ok( '1 >  1' );
evals_true_ok ( '1 >= 0' );
evals_true_ok ( '1 >= 1' );

# parens

evals_ok( "((((((1))))))", 1 );
evals_true_ok( "(((T)))" );
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

# promotion of non-numerics in arithmetic ops

evals_ok( "1 + T", 2 );
evals_ok( "1 * T", 1 );
evals_ok( "1 + F", 1 );
evals_ok( "1 * F", 0 );
evals_ok( '"4" * 1', 4 );
evals_ok( '"fred" * 1', 0 );

# NAs

evals_same_ok( "NA" );
evals_ok( "(NA)", "NA" );
evals_ok( "NA + 1", "NA" );
evals_ok( "NA == NA", "NA" );
evals_ok( "NA != NA", "NA" );

# bad expressions

evals_ok( "1 +", qr/error/ );
evals_ok( "+ 1", qr/error/ );
evals_ok( '"open string', qr/error/ );
evals_ok( "1.a", qr/error/ );
# Now NULL: evals_ok( "BLAH", qr/error/ );

# not

evals_ok( "!T"       , "F" );
evals_ok( "![T,F]"   , "[F,T]" );
evals_ok( "![T,NA,F]", "[F,NA,T]" );
evals_ok( "!NULL",   , qr/error/ );
