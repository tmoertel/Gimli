#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 35;

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


# length

evals_ok( 'length(NULL)'  , 0 );
evals_ok( 'length(1)'     , 1 );
evals_ok( 'length(1,1)'   , 2 );
evals_ok( 'length(1:3,1)' , 4 );


# regex match

evals_ok( 'match("x", "y")'              , 'NULL' );
evals_ok( 'match("x", "x")'              , '["","x",""]' );
evals_ok( 'match("axb", "x")'            , '["a","x","b"]' );
evals_ok( 'match("x", "(x)")'            , '"x"' );
evals_ok( 'match("X", "(x)")'            , 'NULL' );
evals_ok( 'match("X", "(x)", "i")'       , '"X"' );
evals_ok( 'match("Xa", "(x)(.)", "i")'   , '["X","a"]' );
evals_ok( 'match("x\na", "(x.*a)")'      , 'NULL' );
evals_ok( 'match("x\na", "(x.*a)", "s")' , '"x\na"' );
evals_ok( 'match()'                      , qr/error:.* arguments/ );
evals_ok( 'match("")'                    , qr/error:.* arguments/ );
evals_ok( 'match("","","","")'           , qr/error:.* arguments/ );


# names

evals_ok( 'names(table(x=1))', '"x"' );
evals_ok( 'names(table(x=1,y=2))', '["x","y"]' );
evals_ok( 'names(NULL)', qr/error:.* not a table/ );


# uniq

evals_ok( 'uniq(NULL)'       , 'NULL' );
evals_ok( 'uniq(1)'          , '1' );
evals_ok( 'uniq(2,1,2,1)'    , '[2,1]' );
evals_ok( 'uniq([2,1],3,2)'  , '[2,1,3]' );
evals_ok( 'uniq("T",T)'      , '"T"' );
