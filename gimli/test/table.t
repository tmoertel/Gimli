#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 17;

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


# vector projection

my $t = "x <- table(x=1:3,y=11:13,z=c(T,F,T))";

evals_ok( "$t; x\$1", "[1,2,3]" );
evals_ok( "$t; x\$2", "[11,12,13]" );
evals_ok( "$t; x\$3", "[TRUE,FALSE,TRUE]" );
evals_ok( "$t; x\$0", qr/out of range/ );
evals_ok( "$t; x\$4", qr/out of range/ );

evals_ok( "$t; x\$x", "[1,2,3]" );
evals_ok( "$t; x\$y", "[11,12,13]" );
evals_ok( "$t; x\$z", "[TRUE,FALSE,TRUE]" );
evals_ok( "$t; x\$foo", qr/nonexistent column/ );


# table projection

evals_exact_ok( "$t; x\$(x)", <<EOF);
  x
1 1
2 2
3 3
EOF

evals_exact_ok( "$t; x\$(x,y)", <<EOF);
  x  y
1 1 11
2 2 12
3 3 13
EOF

evals_exact_ok( "$t; x\$(y,x)", <<EOF);
   y x
1 11 1
2 12 2
3 13 3
EOF

evals_exact_ok( "$t; x\$(z=x+20,C2=y-10)", <<EOF);
   z C2
1 21  1
2 22  2
3 23  3
EOF
