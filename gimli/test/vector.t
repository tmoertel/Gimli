#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 31;

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

evals_ok( '["s",1,T,F]', '["s","1","TRUE","FALSE"]' );  # string trumps all
evals_ok( '[1,T,F]'    , '[1,1,0]' ); # numeric trumps logical

# arithmetic and comparison ops thread over vectors

evals_ok( "1 + [1,2,3]"         , "[2,3,4]" );
evals_ok( "[2,3,4] - 1"         , "[1,2,3]" );
evals_ok( "1 == [0,1,2]"        , "[FALSE,TRUE,FALSE]" );
evals_ok( "1 != [0,1,2]"        , "[TRUE,FALSE,TRUE]" );
evals_ok( "[10,20,30] + [1,2,3]", "[11,22,33]" );
evals_ok( "[10,20] + [1,2,3]"   , "[11,22,13]" );
evals_ok( "[0,1,2] == [0,1,2]"  , "[TRUE,TRUE,TRUE]" );
evals_ok( "[0,1] == [0,1,2]"    , "[TRUE,TRUE,FALSE]" );

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
evals_ok( "$setup; x[y]"         , "[1,3]" );
evals_ok( "$setup; x[[1,NA,2]]"  , "[1,NA,2]" );
evals_ok( "$setup; x[[T,NA,T]]"  , "[1,NA,3]" );
evals_ok( "$setup; x[x==3]"      , "3" );
evals_ok( "$setup; x[-1]"        , "[2,3,4]" );
evals_ok( "$setup; x[[-1,-3]]"   , "[2,4]" );

# series

evals_ok( "1:1"  , 1 );
evals_ok( "1:3"  , "[1,2,3]" );
evals_ok( "1:3*2", "[2,4,6]" );