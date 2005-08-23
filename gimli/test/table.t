#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 51;

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
evals_ok( "$t; x\$3", "[TRUE,FALSE,TRUE]" );
evals_ok( "$t; x\$0", qr/out of range/ );
evals_ok( "$t; x\$4", qr/out of range/ );

# by column name

evals_ok( "$t; x\$x", "[1,2,3]" );
evals_ok( "$t; x\$y", "[11,12,13]" );
evals_ok( "$t; x\$z", "[TRUE,FALSE,TRUE]" );
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
1 FALSE
2  TRUE
3 FALSE
EOF

evals_exact_ok( "q <- 3; $t; x\$(x=x+q)", <<EOF);
  x
1 4
2 5
3 6
EOF

# star expands to all columns

evals_exact_ok( "$t; x\$(*)", <<EOF);
  x  y     z
1 1 11  TRUE
2 2 12 FALSE
3 3 13  TRUE
EOF

evals_exact_ok( "$t; x\$(*, r=9)", <<EOF);
  x  y     z r
1 1 11  TRUE 9
2 2 12 FALSE 9
3 3 13  TRUE 9
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


#==============================================================================
# table selection
#==============================================================================

evals_exact_ok( "$t; x[T]", <<EOF);
  x  y     z
1 1 11  TRUE
2 2 12 FALSE
3 3 13  TRUE
EOF

evals_exact_ok( "$t; x[F]", <<EOF);
 x y z
EOF

evals_exact_ok( "$t; x[y==12]", <<EOF);
  x  y     z
1 2 12 FALSE
EOF

evals_exact_ok( "$t; x[y!=12]", <<EOF);
  x  y    z
1 1 11 TRUE
2 3 13 TRUE
EOF

evals_exact_ok( "$t; x[z]", <<EOF);
  x  y    z
1 1 11 TRUE
2 3 13 TRUE
EOF

evals_exact_ok( "$t; x[x<-T]", <<EOF);
  x  y     z
1 1 11  TRUE
2 2 12 FALSE
3 3 13  TRUE
EOF

evals_exact_ok( "t <- table(x=1:3,y=[NA,1,3]); t[y==1]", <<EOF);
   x  y
1 NA NA
2  2  1
EOF

evals_ok( "$t; y<-4; x[y<-F]; y", 4);


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

evals_ok( "$t; x\$x + 10 == x\$y"      , "[TRUE,TRUE,TRUE]" );
evals_ok( "$t; x[z]\$x + 10 == x[z]\$y", "[TRUE,TRUE]" );
