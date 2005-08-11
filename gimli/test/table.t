#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 7;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

#==============================================================================
# tests
#==============================================================================

# table constuction

evals_exact_ok( "table(x=5)", <<EOF );
  x
1 5
EOF

evals_exact_ok( "table(x=1,y=2)", <<EOF );
  x y
1 1 2
EOF

evals_exact_ok( "table(x=1:3,y=11:13)", <<EOF );
  x  y
1 1 11
2 2 12
3 3 13
EOF

evals_ok( "table(x=1,y=11:13)", qr/error/ );


# projection

my $t = "x <- table(x=1:3,y=11:13,z=c(T,F,T))";

evals_ok( "$t; x\$1", "[1,2,3]" );
evals_ok( "$t; x\$2", "[11,12,13]" );
evals_ok( "$t; x\$3", "[TRUE,FALSE,TRUE]" );
