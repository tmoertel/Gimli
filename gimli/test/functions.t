#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 23;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

#==============================================================================
# tests
#==============================================================================

evals_ok( '(func() 1)()'                  , 1 );
evals_ok( '(func(x) x + 1)(1)'            , 2 );
evals_ok( '(func(x) x + 1)(x=1)'          , 2 );
evals_ok( '(func(x=3) x + 1)(1)'          , 2 );
evals_ok( '(func(x=3) x + 1)(x=1)'        , 2 );
evals_ok( '(func(x=3) x + 1)()'           , 4 );
evals_ok( '(func(xy=3) xy + 1)(x=1)'      , 2 );
evals_ok( '(func(x,y) 10*x + y)(1,2)'     , 12 );
evals_ok( '(func(x,y) 10*x + y)(x=1,y=2)' , 12 );
evals_ok( '(func(x,y) 10*x + y)(y=2,x=1)' , 12 );
evals_ok( '(func(x,y) 10*x + y)(x=1,2)'   , 12 );
evals_ok( '(func(x,y) 10*x + y)(1,y=2)'   , 12 );
evals_ok( '(func(x,y) 10*x + y)(2,x=1)'   , 12 );
evals_ok( '(func(x,y) 10*x + y)(y=2,1)'   , 12 );

# defaults are evaluated *after* user-given arguments are bound

evals_ok( '(func(x,y=3*x) 10*x + y)(1)'     , 13 );
evals_ok( '(func(x,y=3*x) 10*x + y)(x=1)'   , 13 );
evals_ok( '(func(x,y=3*x) 10*x + y)(1,y=2)' , 12 );

# functions close-over their local environments

evals_ok( <<EOF, "2\n3\n10" );
f <- local do
    x <- 1
    func() x <<- x+1
end;
x <- 10;
f()  # 2
f()  # 3
x    # 10
EOF

evals_ok( <<EOF, "11\n12\n3\n4" );
mk.inc <- local do
  x <- 1
  func(y=x) do
    x <<- x+1
    func() y <<- y+1
  end
end;
f <- mk.inc(10);  # make incrementer w/ seed 10
f() # 11
f() # 12
f <- mk.inc();    # use default seed (now 2)
f() # 3
f() # 4
EOF

# error-reporting

evals_ok( '(func(xy=3,xz) 1)(x=1,2)' , qr/error:.* ambiguous/ );
evals_ok( '(func(x) 1)(y=1)'         , qr/error:.* no .* argument match/ );
evals_ok( '(func(x) 1)()'            , qr/error:.* no value provided for/ );
evals_ok( '(func() 1)(1)'            , qr/error:.* too many arguments/ );
