#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 2;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

#==============================================================================
# tests
#==============================================================================

evals_ok( ":inspect 1",
          "EVal (VVector (V VTNum 1 [SNum 1.0]))" );

evals_ok( ":i 1 + 2",
          "EBinOp BinOpAdd (EVal (VVector (V VTNum 1 [SNum 1.0]))) (EVal (VVector (V VTNum 1 [SNum 2.0])))" );
