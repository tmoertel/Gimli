#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 47;

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


#==============================================================================
# for elem in collection ...
#==============================================================================

# OVER VECTORS

evals_ok( 'for x in NULL do T end', 'NULL' );
evals_ok( 'for x in 1:3 do x + 1 end',  '4' );
evals_ok( 'y <- NULL; for x in 1:3 do y <- [x,y] end', '[3,2,1]' );

# suffix form

evals_ok( 'T for x in NULL', 'NULL' );
evals_ok( 'x+1 for x in 1:3',  '4' );
evals_ok( 'y <- NULL; y <- [x,y] for x in 1:3', '[3,2,1]' );

# OVER LISTS

evals_ok( 'for x in list() do T end', 'NULL' );
evals_ok( 'for x in list(1,2,3) do x + 1 end',  '4' );
evals_ok( 'y <- NULL; for x in list(1,2,3) do y <- [x,y] end', '[3,2,1]' );

# suffix form

evals_ok( 'T for x in list()', 'NULL' );
evals_ok( 'x+1 for x in list(1,2,3)',  '4' );
evals_ok( 'y <- NULL; y <- [x,y] for x in list(1,2,3)', '[3,2,1]' );

#==============================================================================
# local & <<-
#==============================================================================

evals_ok( 'x <- 1; local x <- 2; x' , 1 );
evals_ok( 'local x <- 2'            , 2 );
evals_ok( 'x <- 1; local x <<- 2; x', 2 );
evals_ok( 'local x <<- 2'           , 2 );

evals_ok( <<EOF                     , "0\n2" );
print(x <- 0);
local do
  x <- 1;
  local x <<- 2;
  x
end
EOF

evals_ok( <<EOF                     , "0\n2\n0" );
print(x <- 0);
print(
  local do
    x <- 1;
    local x <<- 2
  end
);
x
EOF

evals_ok( <<EOF                     , "0\n2\n2" );
print(x <- 0);
print(
  local do
    local x <<- 2
  end
);
x
EOF
