#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 16;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

#==============================================================================
# tests
#==============================================================================

my $t = "t <- table(x=1:3,y=[T,F,T])";
my $u = "u <- table(x=2:4,z=[T,F,T])";
my $v = "v <- table(z=1:2)";

join_exact_ok( "t -=- t", <<EOF );
  x y
1 1 T
2 2 F
3 3 T
EOF

join_exact_ok( "t *** v", <<EOF );
  x y z
1 1 T 1
2 1 T 2
3 2 F 1
4 2 F 2
5 3 T 1
6 3 T 2
EOF

join_exact_ok( "t -=- u", <<EOF );
  x y z
1 2 F T
2 3 T F
EOF

join_exact_ok( "t ==- u", <<EOF );
  x y  z
1 1 T NA
2 2 F  T
3 3 T  F
EOF

join_exact_ok( "t -== u", <<EOF );
   x  y x.1 z
1  2  F   2 T
2  3  T   3 F
3 NA NA   4 T
EOF

join_exact_ok( "t === u", <<EOF );
   x  y x.1  z
1  1  T  NA NA
2  2  F   2  T
3  3  T   3  F
4 NA NA   4  T
EOF


# default when no columns match is to join on ROW.ID

join_exact_ok( "t -=- v", <<EOF );
  x y z
1 1 T 1
2 2 F 2
EOF

join_exact_ok( "t{ROW.ID} -=- v", <<EOF );
  x y z
1 1 T 1
2 2 F 2
EOF

join_exact_ok( "t -=- {ROW.ID}v", <<EOF );
  x y z
1 1 T 1
2 2 F 2
EOF

join_exact_ok( "t{ROW.ID} -=- {ROW.ID}v", <<EOF );
  x y z
1 1 T 1
2 2 F 2
EOF




# w/ column expressions

join_exact_ok( "t{x} -=- u", <<EOF );
  x y z
1 2 F T
2 3 T F
EOF

join_exact_ok( "t -=- {x}u", <<EOF );
  x y z
1 2 F T
2 3 T F
EOF

join_exact_ok( "t{x} -=- {x}u", <<EOF );
  x y z
1 2 F T
2 3 T F
EOF

join_exact_ok( "t{x} -=- {x-1}u", <<EOF );
  x y x.1 z
1 1 T   2 T
2 2 F   3 F
3 3 T   4 T
EOF


join_exact_ok( "t{x} -=- {z}v", <<EOF );
  x y
1 1 T
2 2 F
EOF

join_exact_ok( "t{no.such.column} -=- {z}v", <<EOF );
(empty table)
EOF


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
