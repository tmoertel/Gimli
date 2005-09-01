#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 4;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;


#==============================================================================
# tests
#==============================================================================

evals_exact_ok( "SYS.ROWS <- 0; 1", <<EOF );
(0 of 1 lines)
EOF

evals_exact_ok( "SYS.COLS <- 0; 1", <<EOF );
...
EOF

evals_exact_ok( "SYS.ROWS <- 1; 1", <<EOF );
1
EOF

evals_exact_ok( "SYS.COLS <- 1; 1", <<EOF );
1
EOF
