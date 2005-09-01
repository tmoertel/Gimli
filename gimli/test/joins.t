#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 6;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

#==============================================================================
# tests
#==============================================================================

my $t = "t <- table(x=1:3,y=[T,F,T])";
my $u = "u <- table(x=2:4,z=[T,F,T])";
my $v = "v <- table(z=1:2)";

join_exact_ok( "t *** v", <<EOF );
  x y z
1 1 T 1
2 1 T 2
3 2 F 1
4 2 F 2
5 3 T 1
6 3 T 2
EOF

SKIP: {

    skip "only Cartesian-product join implemented", 5;

join_exact_ok( "t === u", <<EOF );
x y z
2 F T
3 T F
EOF

join_exact_ok( "t *== u", <<EOF );
  x y  z
1 1 T NA
2 2 F  T
3 3 T  F
EOF

join_exact_ok( "t ==* u", <<EOF );
  x  y z
1 2  F T
2 3  T F
3 4 NA T
EOF

join_exact_ok( "t *=* u", <<EOF );
  x  y  z
1 1  T NA
2 2  F  T
3 3  T  F
4 4 NA  T
EOF

join_exact_ok( "t *=* u", <<EOF );
  x  y  z
1 1  T NA
2 2  F  T
3 3  T  F
4 4 NA  T
EOF

}

#==============================================================================
# helpers
#==============================================================================

sub join_exact_ok {
    my @args = @_;
    $args[0] = "$t; $u; $v; $args[0]";
    my $level = $Test::Builder::Level;
    {   local $Test::Builder::Level = $level + 1;
        evals_exact_ok(@args);
    }
}
