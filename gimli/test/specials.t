#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 22;

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

#==============================================================================
# unless ... else ... then
#==============================================================================

# basics

evals_ok( "unless T then 1 else 2", 2 );
evals_ok( "unless F then 1 else 2", 1 );
evals_ok( "unless \"hi\" then 1 else 2", 2 );
evals_ok( "unless [] then 1 else 2", 1 );

evals_ok( "unless T then 1", 'T' );
evals_ok( "unless F then 1", '1' );
evals_ok( "unless \"hi\" then 1", "\"hi\"" );
evals_ok( "unless [] then 1", "1" );

# nesting

evals_ok( "unless T then unless F then 1 else 2 else 3", 3 );

# laziness of evaluation

evals_ok( "unless F then 1 else 2\$2", 1 );
evals_ok( "unless T then 1\$1 else 2", 2 );
