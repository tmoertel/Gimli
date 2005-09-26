#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 70;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;


#==============================================================================
# table constuction
#==============================================================================

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


#==============================================================================
# vector projection
#==============================================================================

my $t = "x <- table(x=1:3,y=11:13,z=c(T,F,T))";

# by column number

evals_ok( "$t; x\$1", "[1,2,3]" );
evals_ok( "$t; x\$2", "[11,12,13]" );
evals_ok( "$t; x\$3", "[T,F,T]" );
evals_ok( "$t; x\$0", qr/out of range/ );
evals_ok( "$t; x\$4", qr/out of range/ );

# by column name

evals_ok( "$t; x\$x", "[1,2,3]" );
evals_ok( "$t; x\$y", "[11,12,13]" );
evals_ok( "$t; x\$z", "[T,F,T]" );
evals_ok( "$t; x\$foo", qr/column name .* does not exist/ );


#==============================================================================
# table projection
#==============================================================================

evals_ok( "$t; y <- 4; x\$(y=3); y", 4 );
evals_ok( "$t; y <- 4; x\$(y=3,z=nosuchcolumn); y", 4 );

# check for non-existent column names

evals_ok( "$t; x\$(foo)", qr/column name .* does not exist/ );
evals_ok( "$t; x\$(-foo)", qr/column name .* does not exist/ );
evals_ok( "$t; x\$(x,foo)", qr/column name .* does not exist/ );
evals_ok( "$t; x\$(-x,foo)", qr/column name .* does not exist/ );

# by column number

evals_exact_ok( "$t; x\$(1)", <<EOF);
  x
1 1
2 2
3 3
EOF

evals_exact_ok( "$t; x\$(1,2)", <<EOF);
  x  y
1 1 11
2 2 12
3 3 13
EOF

# by column name

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

evals_exact_ok( "$t; x\$(y,x=x)", <<EOF);
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


# by expression name-value pair

evals_exact_ok( "$t; q <- \"y\"; x\$((q)=x)", <<EOF);
  y
1 1
2 2
3 3
EOF



# inverted

evals_exact_ok( "$t; x\$(-3)", <<EOF);
  x  y
1 1 11
2 2 12
3 3 13
EOF

evals_exact_ok( "$t; x\$(-z)", <<EOF);
  x  y
1 1 11
2 2 12
3 3 13
EOF

evals_exact_ok( "$t; x\$(-z=x)", <<EOF);
  x  y
1 1 11
2 2 12
3 3 13
EOF

evals_exact_ok( "$t; x\$(-2,3)", <<EOF);
  x
1 1
2 2
3 3
EOF

evals_exact_ok( "$t; x\$(-y,z)", <<EOF);
  x
1 1
2 2
3 3
EOF

evals_exact_ok( "$t; x\$(x = y==12)", <<EOF);
  x
1 F
2 T
3 F
EOF

evals_exact_ok( "q <- 3; $t; x\$(x=x+q)", <<EOF);
  x
1 4
2 5
3 6
EOF

# star expands to all columns

evals_exact_ok( "$t; x\$(*)", <<EOF);
  x  y z
1 1 11 T
2 2 12 F
3 3 13 T
EOF

evals_exact_ok( "$t; x\$(*, r=9)", <<EOF);
  x  y z r
1 1 11 T 9
2 2 12 F 9
3 3 13 T 9
EOF

# inverted star expands to no columns

evals_exact_ok( "$t; x\$(-3,*)", <<EOF);
  x  y
1 1 11
2 2 12
3 3 13
EOF

evals_exact_ok( "$t; x\$(-*,z)", <<EOF);
  x  y
1 1 11
2 2 12
3 3 13
EOF

evals_exact_ok( "$t; x\$(-*,*,*,*,z=x)", <<EOF);
  x  y
1 1 11
2 2 12
3 3 13
EOF

evals_exact_ok( "$t; x\$(-2,*,3)", <<EOF);
  x
1 1
2 2
3 3
EOF

# pspecs that contain expressions

evals_exact_ok( "$t; x\$(-[1:2])", <<EOF );
  z
1 T
2 F
3 T
EOF

evals_exact_ok( "$t; x\$((1+1+1))", <<EOF );
  z
1 T
2 F
3 T
EOF

evals_exact_ok( "$t; x\$(c(2,1))", <<EOF);
   y x
1 11 1
2 12 2
3 13 3
EOF

evals_exact_ok( "$t; x\$(-c(2,1))", <<EOF);
  z
1 T
2 F
3 T
EOF

evals_exact_ok( "$t; x\$(\"z\")", <<EOF);
  z
1 T
2 F
3 T
EOF

evals_exact_ok( "$t; q <- [\"x\",\"y\"]; x\$((q))", <<EOF);
  x  y
1 1 11
2 2 12
3 3 13
EOF


evals_ok( "$t; x\$((NULL))", qr/error: .* vector/x );


# additive overlay projection

evals_exact_ok( "$t; x\$(+x=x-1)", <<EOF);
  x  y z
1 0 11 T
2 1 12 F
3 2 13 T
EOF

evals_exact_ok( "$t; x\$(+y=x,x=y)", <<EOF);
   x y z
1 11 1 T
2 12 2 F
3 13 3 T
EOF

evals_exact_ok( "$t; x\$(+x=x-1, q=x)", <<EOF);
  x  y z q
1 0 11 T 1
2 1 12 F 2
3 2 13 T 3
EOF

evals_exact_ok( "$t; x\$(+\"x\"=x-1, q=x)", <<EOF);
  x  y z q
1 0 11 T 1
2 1 12 F 2
3 2 13 T 3
EOF


# serial projection

evals_exact_ok( "$t; x\$(-x;-y)", <<EOF);
  z
1 T
2 F
3 T
EOF

evals_exact_ok( "$t; x\$(-x; *,x=1; -y)", <<EOF);
  z x
1 T 1
2 F 1
3 T 1
EOF


#==============================================================================
# table selection
#==============================================================================

evals_exact_ok( "$t; x[T]", <<EOF);
  x  y z
1 1 11 T
2 2 12 F
3 3 13 T
EOF

evals_exact_ok( "$t; x[F]", <<EOF);
 x y z
EOF

evals_exact_ok( "$t; x[y==12]", <<EOF);
  x  y z
1 2 12 F
EOF

evals_exact_ok( "$t; x[y!=12]", <<EOF);
  x  y z
1 1 11 T
2 3 13 T
EOF

evals_exact_ok( "$t; x[z]", <<EOF);
  x  y z
1 1 11 T
2 3 13 T
EOF

evals_exact_ok( "$t; x[x<-T]", <<EOF);
  x  y z
1 1 11 T
2 2 12 F
3 3 13 T
EOF

evals_exact_ok( "t <- table(x=1:3,y=[NA,1,3]); t[y==1]", <<EOF);
  x y
1 2 1
EOF

evals_ok( "$t; y<-4; x[y<-F]; y", 4);

evals_exact_ok( "$t; x[ROW.ID %in% [1,3]]", <<EOF);
  x  y z
1 1 11 T
2 3 13 T
EOF


#==============================================================================
# complex operations
#==============================================================================

evals_ok( "$t; x[x==2]\$y", 12 );
evals_ok( "$t; x[x!=2]\$y", "[11,13]" );
evals_exact_ok( "$t; x[x!=2]\$(y)", <<EOF);
   y
1 11
2 13
EOF

evals_ok( "$t; x\$x + 10 == x\$y"      , "[T,T,T]" );
evals_ok( "$t; x[z]\$x + 10 == x[z]\$y", "[T,T]" );


#==============================================================================
# column-name uniqueness
#==============================================================================

evals_ok( "table(x=1,x=1)[F]"      , "x x.1" );
evals_ok( "table(x=1,x=1,x=1)[F]"  , "x x.1 x.2" );
evals_ok( "table(x=1,x=1,x.1=1)[F]", "x x.2 x.1" );
evals_ok( "table(x.1=1,x.1=1)[F]"  , "x.1 x.1.1" );
