#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 60;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

#==============================================================================
# tests
#==============================================================================

# as.list

evals_ok( 'as.list(NULL)',      , 'list()' );
evals_ok( 'as.list(1)',         , '[1] => 1' );
evals_ok( 'as.list(NULL,1)'     , '[1] => 1' );
evals_exact_ok( 'as.list([1,3])',  <<'EOF' );
[1] => 1
[2] => 3
EOF
evals_exact_ok( 'as.list(1,"q")',  <<'EOF' );
[1] => 1
[2] => "q"
EOF
evals_ok( 'as.list(table(x=2))' , '$x => 2' );
evals_ok( 'as.list(list(x=2))'  , '$x => 2' );


# as.table

evals_ok( 'as.table(NULL)',      , qr/error/ );
evals_ok( 'as.table(list())',    , qr/error/ );
evals_exact_ok( "as.table(list(x=1))", <<EOF );
  x
1 1
EOF
evals_exact_ok( "as.table(NULL,list(),list(x=1))", <<EOF );
  x
1 1
EOF
evals_exact_ok( "as.table(1)", <<EOF );
  NA
1  1
EOF
evals_exact_ok( "as.table(list(1,2))", <<EOF );
  NA NA.1
1  1    2
EOF
evals_exact_ok( "as.table(1,2)", <<EOF );
  NA NA.1
1  1    2
EOF
evals_exact_ok( "as.table(table(x=1))", <<EOF );
  x
1 1
EOF
evals_exact_ok( "as.table(table(x=1,y=2))", <<EOF );
  x y
1 1 2
EOF
evals_exact_ok( "as.table(table(x=1),list(y=2))", <<EOF );
  x y
1 1 2
EOF


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


# inspect

evals_ok( 'inspect(1)', "1 => 1\n1" );


# length

evals_ok( 'length(NULL)'  , 0 );
evals_ok( 'length(1)'     , 1 );
evals_ok( 'length(1,1)'   , 2 );
evals_ok( 'length(1:3,1)' , 4 );


# match

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
evals_ok( 'names(NULL)', qr/error:/ );


# print

evals_ok( 'print(1)'    , "1\n1" );
evals_ok( 'print("1")'  , qq("1"\n"1") );


# sort

evals_ok( 'sort(3,1,2)',      , '[1,2,3]'   );
evals_ok( 'sort([3,1],[2,4])' , '[1,2,3,4]' );


# uniq

evals_ok( 'uniq(NULL)'       , 'NULL' );
evals_ok( 'uniq(1)'          , '1' );
evals_ok( 'uniq(2,1,2,1)'    , '[2,1]' );
evals_ok( 'uniq([2,1],3,2)'  , '[2,1,3]' );
evals_ok( 'uniq("T",T)'      , '"T"' );


# var

evals_ok( 'x <- 1; var("x")'         , 1 );
evals_ok( 'x <- 1; y <- "x"; var(y)' , 1 );
evals_ok( 'var("x")'                 , qr/error:.* not found/ );

