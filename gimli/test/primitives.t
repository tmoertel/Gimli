#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 19;

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
evals_ok( 'glob("*/p?im?ti?e?.t")', '"./test/primitives.t"' );
evals_ok( 'glob("*/NO_SUCH_FILE")', 'NULL' );
evals_ok( 'glob("*/primitives.t","*/NO_SUCH_FILE")', '"./test/primitives.t"' );
evals_ok( 'glob(1)', qr/error.* not a string/ );
evals_ok( 'glob()', 'NULL' );


# uniq

evals_ok( 'uniq(NULL)'       , 'NULL' );
evals_ok( 'uniq(1)'          , '1' );
evals_ok( 'uniq(2,1,2,1)'    , '[2,1]' );
evals_ok( 'uniq([2,1],3,2)'  , '[2,1,3]' );
evals_ok( 'uniq("T",T)'      , '"T"' );


# names

evals_ok( 'names(table(x=1))', '"x"' );
evals_ok( 'names(table(x=1,y=2))', '["x","y"]' );
evals_ok( 'names(NULL)', qr/error:.* not a table/ );
