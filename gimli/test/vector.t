#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 46;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

#==============================================================================
# tests
#==============================================================================

# two vector-construction syntaxes

evals_same_ok( "[1,2]" , "[...] vector syntax" );
evals_ok(      "c(1,2)", "[1,2]", "c(...) vector syntax" );

# scalars are really single-element vectors

evals_ok( "[1]" , 1 );
evals_ok( "c(1)", 1 );

# elements of a vector are coerced into like types

evals_ok( '["s",1,T,F]', '["s","1","T","F"]' );  # string trumps all
evals_ok( '[1,T,F]'    , '[1,1,0]' ); # numeric trumps logical

# nested vectors are flattened

evals_ok( '[1,[2,[3]]]'              , '[1,2,3]' );
evals_ok( 'x <- 3; [1,[2,x]]'        , '[1,2,3]' );
evals_ok( 'x <- [3,4]; [1,[2,x]]'    , '[1,2,3,4]' );
evals_ok( 'c(1,[2,c(3)])'            , '[1,2,3]' );
evals_ok( 'x <- 3; c(1,[2,x])'       , '[1,2,3]' );
evals_ok( 'x <- c(3,4); c(1,[2,x])'  , '[1,2,3,4]' );

# error handling: vector-constructor's arguments must be vectors or scalars

evals_ok( '[table(x=1)]',    qr/error.*not a vector/ );

# arithmetic and comparison ops thread over vectors

evals_ok( "1 + [1,2,3]"         , "[2,3,4]" );
evals_ok( "[2,3,4] - 1"         , "[1,2,3]" );
evals_ok( "1 == [0,1,2]"        , "[F,T,F]" );
evals_ok( "1 != [0,1,2]"        , "[T,F,T]" );
evals_ok( "[10,20,30] + [1,2,3]", "[11,22,33]" );
evals_ok( "[10,20] + [1,2,3]"   , "[11,22,13]" );
evals_ok( "[0,1,2] == [0,1,2]"  , "[T,T,T]" );
evals_ok( "[0,1] == [0,1,2]"    , "[T,T,F]" );

# NAs are preserved

evals_ok( "[NA]"       , "NA" );
evals_ok( "[1,NA]"     , "[1,NA]" );
evals_ok( "NA + [1,2]" , "[NA,NA]" );
evals_ok( "NA == [1,2]", "[NA,NA]" );

# vector selection

my $setup = "x <- [1,2,3,4]; y <- [1,3]";

evals_ok( "$setup; x[1]"         , "1" );
evals_ok( "$setup; x[3]"         , "3" );
evals_ok( "$setup; x[[3,1]]"     , "[3,1]" );
evals_ok( "$setup; x[c(T,F,F,T)]", "[1,4]" );
evals_ok( "$setup; x[c(T,F)]"    , "[1,3]" );
evals_ok( "$setup; x[y]"         , "[1,3]" );
evals_ok( "$setup; x[[1,NA,2]]"  , "[1,NA,2]" );
evals_ok( "$setup; x[[T,NA,T]]"  , "[1,NA,3,4]" );
evals_ok( "$setup; x[x==3]"      , "3" );
evals_ok( "$setup; x[-1]"        , "[2,3,4]" );
evals_ok( "$setup; x[2:4]"       , "[2,3,4]" );
evals_ok( "$setup; x[-3 : -1]"   , "4" );
evals_ok( "$setup; x[[-1,-3]]"   , "[2,4]" );

# series

evals_ok( "1:1"                , 1 );
evals_ok( "1:3"                , "[1,2,3]" );
evals_ok( "1:3*2"              , "[2,4,6]" );
evals_ok( "x <- 1; y <- 3; x:y", "[1,2,3]" );
evals_ok( "[1,2,3]:[4,5]"      , "[1,2,3,4]" );
evals_ok( "T:T"                , 1 );
evals_ok( "F:T"                , "[0,1]" );
evals_ok( "1:-1"               , "NULL" );
