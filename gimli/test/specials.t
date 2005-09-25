#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 28;

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

# suffix form

evals_ok("1 if T", '1' );
evals_ok("1 if F", 'F' );
evals_ok("1 if [] if F", 'F' );  # rightmost is outermost


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

# suffix form

evals_ok("1 unless T", 'T' );
evals_ok("1 unless F", '1' );
evals_ok("1 unless 3 unless 2", '2' );  # rightmost is outermost
