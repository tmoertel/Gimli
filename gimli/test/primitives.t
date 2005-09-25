#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 10;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

#==============================================================================
# tests
#==============================================================================

# is.na

evals_ok( "is.na([])"          , 'F' );
evals_ok( "is.na(1)"           , 'F' );
evals_ok( "is.na(NA)"          , 'T' );
evals_ok( "is.na([1,NA,3])"    , '[F,T,F]' );
evals_ok( "is.na(table(x=1))"  , 'F' );


# glob

evals_ok( 'glob("*/primitives.t")', '"./test/primitives.t"' );
evals_ok( 'glob("*/NO_SUCH_FILE")', 'NULL' );
evals_ok( 'glob("*/primitives.t","*/NO_SUCH_FILE")', '"./test/primitives.t"' );
evals_ok( 'glob(1)', qr/error.* not a string/ );
evals_ok( 'glob()', 'NULL' );
