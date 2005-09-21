#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 8;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

unlike( run_gimli(":quit"), qr/\S/, "quit" );
unlike( run_gimli(":q"), qr/\S/, "quit" );
like( run_gimli(":ASDFASDFASDF"), qr/Unknown command/, "unknown command" );
like( run_gimli(":?"), qr/Commands I know/, "help" );
like( run_gimli(":?\n:?"), qr/Commands I know.*Commands I know/s,
      "two commands" );

evals_ok( "1" , "1" );
evals_ok( "1;", ""  );

# prompt continuation

evals_ok( "1+\n2", 3 );
