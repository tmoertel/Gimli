#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 11;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

#==============================================================================
# if ... else ... then
#==============================================================================

# basics

evals_ok( "if T then 1 else 2", 1 );
evals_ok( "if F then 1 else 2", 2 );
evals_ok( "if \"hi\" then 1 else 2", 1 );
evals_ok( "if [] then 1 else 2", 2 );

evals_ok( "if T then 1", 1 );
evals_ok( "if F then 1", 'F' );
evals_ok( "if \"hi\" then 1", 1 );
evals_ok( "if [] then 1", "NULL" );

# nesting

evals_ok( "if T then if F then 1 else 2 else 3", 2 );

# laziness of evaluation

evals_ok( "if T then 1 else 2\$2", 1 );  # 2$2 (which is illegal) not evaluated
evals_ok( "if F then 1\$1 else 2", 2 );  # 1$1 (which is illegal) not evaluated
