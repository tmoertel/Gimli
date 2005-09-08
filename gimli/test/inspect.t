#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 5;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

#==============================================================================
# tests
#==============================================================================

evals_ok( ":inspect 1"      , "1" );
evals_ok( ":i 1 + 2"        , "1 + 2" );
evals_ok( ":i [1,2]"        , "[1,2]" );
evals_ok( ":i table(x=1)"   , "table(x=1)" );
evals_ok( "x <- 1 + 2\n:e x", "3\n1 + 2" );
