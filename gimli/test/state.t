#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 12;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

#==============================================================================
# tests
#==============================================================================

# multiple expressions

evals_ok( "1;2;3"  , 3 );
evals_ok( "1; 2; 4", 4 );
evals_ok( "1 ; 2"  , 2 );

# binding and variable references

evals_ok( "x <- 1; x"          , 1 );
evals_ok( "2 -> x; x"          , 2 );
evals_ok( "x <- 2; x <- x+x; x", 4 );

evals_ok( "x <- y <- 1; x", 1 );  # <- is right associative
evals_ok( "x <- y <- 2; y", 2 );
evals_ok( "3 -> y -> x; x", 3 );  # -> is left associative
evals_ok( "4 -> y -> x; y", 4 );

# special-name binding

evals_ok( '"x" <- 1; x'          , 1 );
evals_ok( '"!!" <- 1; var("!!")' , 1 );
